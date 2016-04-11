!WRF:DRIVER_LAYER:CONFIGURATION
!

MODULE module_scalar_tables
  USE module_driver_constants
  USE module_state_description
  USE module_domain_type, ONLY : streamrec
#include "scalar_tables.inc"
CONTAINS
  SUBROUTINE init_module_scalar_tables
     INTEGER i , j
     DO j = 1, max_domains
#include "scalar_tables_init.inc"
     END DO
  END SUBROUTINE init_module_scalar_tables
END MODULE module_scalar_tables

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
#include "namelist_defines.inc"
   END TYPE model_config_rec_type

   TYPE grid_config_rec_type
#include "namelist_defines2.inc"
   END TYPE grid_config_rec_type

   TYPE(model_config_rec_type) :: model_config_rec

!#include "scalar_tables.inc"

! special entries (put here but not enshrined in Registry for one reason or other)

!   CHARACTER (LEN=256) :: mminlu = ' '             ! character string for landuse table

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

      INTEGER              :: io_status
      INTEGER              :: i

      LOGICAL              :: nml_read_error

      CHARACTER (LEN=1024) :: nml_name

      INTEGER, PARAMETER :: nml_write_unit= 9
      INTEGER, PARAMETER :: nml_read_unit = 10


! define as temporaries
#include "namelist_defines.inc"

! Statements that specify the namelists
#include "namelist_statements.inc"

      OPEN ( UNIT   = nml_read_unit    ,      &
             FILE   = "namelist.input" ,      &
             FORM   = "FORMATTED"      ,      &
             STATUS = "OLD"            ,      &
             IOSTAT = io_status         )

      IF ( io_status .NE. 0 ) THEN
        CALL WRF_ERROR_FATAL ( 'ERROR OPENING namelist.input' )
      ENDIF

#ifndef NO_NAMELIST_PRINT
      OPEN ( UNIT   = nml_write_unit    ,      &
#if (DA_CORE == 1)
             FILE   = "namelist.output.da" ,      &
#else
             FILE   = "namelist.output" ,      &
#endif
             FORM   = "FORMATTED"      ,      &
             STATUS = "REPLACE"        ,      &
             IOSTAT = io_status         )

      IF ( io_status .NE. 0 ) THEN
#if (DA_CORE == 1)
        CALL WRF_ERROR_FATAL ( 'ERROR OPENING namelist.output.da' )
#else
        CALL WRF_ERROR_FATAL ( 'ERROR OPENING namelist.output' )
#endif
      ENDIF
#endif

! Statements that set the namelist vars to default vals
#  include "namelist_defaults.inc"

#if (DA_CORE == 1)
! Override the default values, because we can not assigned a arrary with different values in registry.

      as1(1:3) = (/ 0.25, 1.0, 1.5 /)
      as2(1:3) = (/ 0.25, 1.0, 1.5 /)
      as3(1:3) = (/ 0.25, 1.0, 1.5 /)
      as4(1:3) = (/ 0.25, 1.0, 1.5 /)
      as5(1:3) = (/ 0.25, 1.0, 1.5 /)
#endif

! Statements that read the namelist are in this file
#  include "config_reads.inc"

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
#include "config_assigns.inc"


      CLOSE ( UNIT = nml_read_unit , IOSTAT = io_status )

      IF ( io_status .NE. 0 ) THEN
        CALL WRF_ERROR_FATAL ( 'ERROR CLOSING namelist.input' )
      ENDIF

#ifndef NO_NAMELIST_PRINT
      CLOSE ( UNIT = nml_write_unit , IOSTAT = io_status )

      IF ( io_status .NE. 0 ) THEN
        CALL WRF_ERROR_FATAL ( 'ERROR CLOSING namelist.output' )
      ENDIF
#endif

#ifdef _WIN32
      model_config_rec%nocolons = .TRUE.   ! always no colons for Windows
#endif

      RETURN

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
        "get_config_rec_as_buffer: buffer size too small for config_rec" )
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
        "get_config_rec_as_buffer: buffer size too small for config_rec" )
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
#include "config_assigns.inc"
   END SUBROUTINE model_to_grid_config_rec


   FUNCTION in_use_for_config ( id, vname ) RESULT ( in_use )
     INTEGER, INTENT(IN) :: id
     CHARACTER*(*), INTENT(IN) :: vname
     LOGICAL in_use
     INTEGER uses

     uses = 0
     in_use = .TRUE.

     IF      ( vname(1:1) .EQ. 'a' ) THEN
#    include "in_use_for_config_a.inc"
     ELSE IF ( vname(1:1) .EQ. 'b' ) THEN
#    include "in_use_for_config_b.inc"
     ELSE IF ( vname(1:1) .EQ. 'c' ) THEN
#    include "in_use_for_config_c.inc"
     ELSE IF ( vname(1:1) .EQ. 'd' ) THEN
#    include "in_use_for_config_d.inc"
     ELSE IF ( vname(1:1) .EQ. 'e' ) THEN
#    include "in_use_for_config_e.inc"
     ELSE IF ( vname(1:1) .EQ. 'f' ) THEN
#    include "in_use_for_config_f.inc"
     ELSE IF ( vname(1:1) .EQ. 'g' ) THEN
#    include "in_use_for_config_g.inc"
     ELSE IF ( vname(1:1) .EQ. 'h' ) THEN
#    include "in_use_for_config_h.inc"
     ELSE IF ( vname(1:1) .EQ. 'i' ) THEN
#    include "in_use_for_config_i.inc"
     ELSE IF ( vname(1:1) .EQ. 'j' ) THEN
#    include "in_use_for_config_j.inc"
     ELSE IF ( vname(1:1) .EQ. 'k' ) THEN
#    include "in_use_for_config_k.inc"
     ELSE IF ( vname(1:1) .EQ. 'l' ) THEN
#    include "in_use_for_config_l.inc"
     ELSE IF ( vname(1:1) .EQ. 'm' ) THEN
#    include "in_use_for_config_m.inc"
     ELSE IF ( vname(1:1) .EQ. 'n' ) THEN
#    include "in_use_for_config_n.inc"
     ELSE IF ( vname(1:1) .EQ. 'o' ) THEN
#    include "in_use_for_config_o.inc"
     ELSE IF ( vname(1:1) .EQ. 'p' ) THEN
#    include "in_use_for_config_p.inc"
     ELSE IF ( vname(1:1) .EQ. 'q' ) THEN
#    include "in_use_for_config_q.inc"
     ELSE IF ( vname(1:1) .EQ. 'r' ) THEN
#    include "in_use_for_config_r.inc"
     ELSE IF ( vname(1:1) .EQ. 's' ) THEN
#    include "in_use_for_config_s.inc"
     ELSE IF ( vname(1:1) .EQ. 't' ) THEN
#    include "in_use_for_config_t.inc"
     ELSE IF ( vname(1:1) .EQ. 'u' ) THEN
#    include "in_use_for_config_u.inc"
     ELSE IF ( vname(1:1) .EQ. 'v' ) THEN
#    include "in_use_for_config_v.inc"
     ELSE IF ( vname(1:1) .EQ. 'w' ) THEN
#    include "in_use_for_config_w.inc"
     ELSE IF ( vname(1:1) .EQ. 'x' ) THEN
#    include "in_use_for_config_x.inc"
     ELSE IF ( vname(1:1) .EQ. 'y' ) THEN
#    include "in_use_for_config_y.inc"
     ELSE IF ( vname(1:1) .EQ. 'z' ) THEN
#    include "in_use_for_config_z.inc"
     ENDIF

     RETURN
   END FUNCTION


! Include the definitions of all the routines that return a namelist values
! back to the driver. These are generated by the registry

   SUBROUTINE init_module_configure
     USE module_scalar_tables
     IMPLICIT NONE
     CALL init_module_scalar_tables
   END SUBROUTINE init_module_configure

   SUBROUTINE wrf_alt_nml_obsolete (nml_read_unit, nml_name)
!
!<DESCRIPTION>
! If there is an error reading the "nml_name" namelist, this routine is
! called to check for namelist variables that have been removed by the 
! developers and are still in user's namelists.
! 
! The calls to this routine are in registry-generated code: inc/config_reads.inc
!</DESCRIPTION>
!
     IMPLICIT NONE
     INTEGER, INTENT(IN)       :: nml_read_unit
     CHARACTER*(*), INTENT(IN) :: nml_name
     INTEGER                   :: nml_error

#include "namelist_defines.inc"
#include "namelist_statements.inc"

! These are the variables that have been removed
     logical , DIMENSION(max_domains) :: pd_moist, pd_chem, pd_tke, pd_scalar
     NAMELIST /dynamics/                 pd_moist, pd_chem, pd_tke, pd_scalar

     integer , DIMENSION(max_domains) :: ucmcall
     NAMELIST /physics/                  ucmcall

     integer , DIMENSION(max_domains) :: obs_nobs_prt
     NAMELIST /fdda/                     obs_nobs_prt

     LOGICAL ::         global, print_detail_airep, print_detail_timing
     NAMELIST /wrfvar1/ global, print_detail_airep, print_detail_timing

     LOGICAL ::         write_qcw, write_qrn, write_qci, write_qsn
     NAMELIST /wrfvar2/ write_qcw, write_qrn, write_qci, write_qsn
     LOGICAL ::          write_qgr, write_filtered_obs
     NAMELIST /wrfvar2/  write_qgr, write_filtered_obs

     LOGICAL ::         use_eos_radobs
     NAMELIST /wrfvar4/ use_eos_radobs

     LOGICAL             :: use_crtm_kmatrix_fast
     NAMELIST /wrfvar14/    use_crtm_kmatrix_fast
     CHARACTER (LEN=256) :: spccoeff_file, taucoeff_file, aerosolcoeff_file
     NAMELIST /wrfvar14/    spccoeff_file, taucoeff_file, aerosolcoeff_file
     CHARACTER (LEN=256) :: cloudcoeff_file, emiscoeff_file
     NAMELIST /wrfvar14/    cloudcoeff_file, emiscoeff_file



! Read the namelist again, if it succeeds after adding the above variables,
! it probably failed because these are still in the namelist.  If it fails
! again, we will return.

     REWIND ( UNIT = nml_read_unit )

!----------------------------- dynamics ---------------------------------
     if ( TRIM(nml_name) .eq. "dynamics" ) then

        READ   ( UNIT = nml_read_unit , NML = dynamics , iostat=nml_error )

        IF ( nml_error .EQ. 0 ) then    ! Successul, rm variables must be problem
           CALL wrf_debug(0, "-- Are pd_moist, pd_chem, pd_tke, or pd_scalar still in your "// &
                               TRIM(nml_name)//" namelist?")
           CALL wrf_debug(0, "-- Replace them with moist_adv_opt, chem_adv_opt, tke_adv_opt "// &
                             " and scalar_adv_opt, respectively.")
        ENDIF

!---------------------------------- physics -----------------------------
     else if ( TRIM(nml_name) .eq. "physics" ) then

        READ   ( UNIT = nml_read_unit , NML = physics , iostat=nml_error )

        IF ( nml_error .EQ. 0 ) then    ! Successul, rm variables must be problem
           CALL wrf_debug(0, "-- Is ucmcall still in your "// TRIM(nml_name)//" namelist?")
           CALL wrf_debug(0, "-- Replace it with sf_urban_physics")
        ENDIF

!---------------------------------- fdda --------------------------------
     else if ( TRIM(nml_name) .eq. "fdda" ) then

        READ   ( UNIT = nml_read_unit , NML = fdda , iostat=nml_error )

        IF ( nml_error .EQ. 0 ) then    ! Successul, rm variables must be problem
           CALL wrf_debug(0, "-- Is obs_nobs_prt still in your "// TRIM(nml_name)//" namelist?")
           CALL wrf_debug(0, "-- Replace it with obs_prt_max")
        ENDIF

!---------------------------------- wrfvar1 -----------------------------
     else if ( TRIM(nml_name) .eq. "wrfvar1" ) then

        READ   ( UNIT = nml_read_unit , NML = wrfvar1 , iostat=nml_error )

        IF ( nml_error .EQ. 0 ) then    ! Successul, rm variables must be problem
           CALL wrf_debug(0, "-- Are global, print_detail_airep, print_detail_timing still in your "// &
                              TRIM(nml_name)//" namelist?")
           CALL wrf_debug(0, "-- Remove global, print_detail_airep, print_detail_timing "// &
                             "from wrfvar1 namelist as they are obsolete.")
        ENDIF

!---------------------------------- wrfvar2 -----------------------------
     else if ( TRIM(nml_name) .eq. "wrfvar2" ) then

        READ   ( UNIT = nml_read_unit , NML = wrfvar2 , iostat=nml_error )

        IF ( nml_error .EQ. 0 ) then    ! Successul, rm variables must be problem
           CALL wrf_debug(0, "-- Are write_qcw, write_qrn, write_qci, write_qsn, write_qgr, "// &
                             "write_filtered_obs still in your "// &
                              TRIM(nml_name)//" namelist?")
           CALL wrf_debug(0, "-- Remove write_qcw, write_qrn, write_qci, write_qsn, write_qgr, "// &
                             "write_filtered_obs as they are obsolete.")
        ENDIF

!---------------------------------- wrfvar4 -----------------------------
     else if ( TRIM(nml_name) .eq. "wrfvar4" ) then

        READ   ( UNIT = nml_read_unit , NML = wrfvar4 , iostat=nml_error )

        IF ( nml_error .EQ. 0 ) then    ! Successul, rm variables must be problem
           CALL wrf_debug(0, "-- Is use_eos_radobs still in your "// &
                              TRIM(nml_name)//" namelist?")
           CALL wrf_debug(0, "-- Remove use_eos_radobs as it is obsolete.")
        ENDIF

!---------------------------------- wrfvar14 -----------------------------
     else if ( TRIM(nml_name) .eq. "wrfvar14" ) then

     READ   ( UNIT = nml_read_unit , NML = wrfvar14 , iostat=nml_error )

        IF ( nml_error .EQ. 0 ) then    ! Successul, rm variables must be problem
           CALL wrf_debug(0, "-- Are use_crtm_kmatrix_fast, spccoeff_file, taucoeff_file, "// &
                             "aerosolcoeff_file, cloudcoeff_file, emiscoeff_file still in your "// &
                              TRIM(nml_name)//" namelist?")
           CALL wrf_debug(0, "-- Remove them as they are obsolete.")
        ENDIF

!---------------------------------- error -------------------------------
     else
         IF ( &
#include "namelist_nametest.inc"
              ) THEN
            nml_error = 0
         ELSE
            CALL wrf_debug(0, TRIM(nml_name)//" is not a valid namelist name")
         ENDIF
     end if

     IF ( nml_error .NE. 0 ) then    ! Still failed
        return
     ENDIF

   END SUBROUTINE wrf_alt_nml_obsolete

END MODULE module_configure


SUBROUTINE set_scalar_indices_from_config ( idomain , dummy2, dummy1 )
  USE module_driver_constants
  USE module_state_description
  USE module_wrf_error
  USE module_configure, ONLY : model_config_rec
  USE module_scalar_tables
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

#include "scalar_indices.inc"
#include "scalar_indices_init.inc"
  RETURN
END SUBROUTINE set_scalar_indices_from_config

