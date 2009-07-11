MODULE module_alloc_space
CONTAINS
   SUBROUTINE alloc_space_field_core ( grid,   id, setinitval_in ,  tl_in , inter_domain_in ,   &
                                  sd31, ed31, sd32, ed32, sd33, ed33, &
                                  sm31 , em31 , sm32 , em32 , sm33 , em33 , &
                                  sm31x, em31x, sm32x, em32x, sm33x, em33x, &
                                  sm31y, em31y, sm32y, em32y, sm33y, em33y )

      USE module_domain_type
      USE module_configure, ONLY : model_config_rec, in_use_for_config
      USE module_state_description

      IMPLICIT NONE

      !  Input data.

      TYPE(domain)               , POINTER          :: grid
      INTEGER , INTENT(IN)            :: id
      INTEGER , INTENT(IN)            :: setinitval_in   ! 3 = everything, 1 = arrays only, 0 = none
      INTEGER , INTENT(IN)            :: sd31, ed31, sd32, ed32, sd33, ed33
      INTEGER , INTENT(IN)            :: sm31, em31, sm32, em32, sm33, em33
      INTEGER , INTENT(IN)            :: sm31x, em31x, sm32x, em32x, sm33x, em33x
      INTEGER , INTENT(IN)            :: sm31y, em31y, sm32y, em32y, sm33y, em33y

      ! this argument is a bitmask. First bit is time level 1, second is time level 2, and so on.
      ! e.g. to set both 1st and second time level, use 3
      !      to set only 1st                        use 1
      !      to set only 2st                        use 2
      INTEGER , INTENT(IN)            :: tl_in
 
      ! true if the allocation is for an intermediate domain (for nesting); only certain fields allocated
      ! false otherwise (all allocated, modulo tl above)
      LOGICAL , INTENT(IN)            :: inter_domain_in

      !  Local data.
      INTEGER idum1, idum2, spec_bdy_width
      INTEGER num_bytes_allocated
      REAL    initial_data_value
      CHARACTER (LEN=256) message
      INTEGER tl
      LOGICAL inter_domain
      INTEGER setinitval
      INTEGER sr_x, sr_y

      !declare ierr variable for error checking ALLOCATE calls
      INTEGER ierr

      INTEGER                              :: loop

      CALL nl_get_sr_x( id , sr_x )
      CALL nl_get_sr_x( id , sr_y )

      tl = tl_in
      inter_domain = inter_domain_in

#if ( RWORDSIZE == 8 )
      initial_data_value = 0.
#else
      CALL get_initial_data_value ( initial_data_value )
#endif

#ifdef NO_INITIAL_DATA_VALUE
      setinitval = 0
#else
      setinitval = setinitval_in
#endif

      CALL nl_get_spec_bdy_width( 1, spec_bdy_width )

      CALL set_scalar_indices_from_config( id , idum1 , idum2 )

      num_bytes_allocated = 0 

#if (EM_CORE == 1)
      IF ( grid%id .EQ. 1 ) CALL wrf_message ( &
          'DYNAMICS OPTION: Eulerian Mass Coordinate ')
#endif
#if (NMM_CORE == 1)
      IF ( grid%id .EQ. 1 ) &
          CALL wrf_message ( 'DYNAMICS OPTION: nmm dyncore' )
#endif
#if (COAMPS_CORE == 1)
        IF ( grid%id .EQ. 1 ) &
          CALL wrf_message ( 'DYNAMICS OPTION: coamps dyncore' )
#endif

# include <allocs.inc>

      IF ( .NOT. grid%have_displayed_alloc_stats ) THEN
        ! we do not want to see this message more than once, as can happen with the allocation and
        ! deallocation of intermediate domains used in nesting.
        WRITE(message,*)&
            'alloc_space_field: domain ',id,', ',num_bytes_allocated,' bytes allocated'
        CALL  wrf_debug( 0, message )
        grid%have_displayed_alloc_stats = .TRUE.   
      ENDIF

   END SUBROUTINE alloc_space_field_core
END MODULE module_alloc_space

