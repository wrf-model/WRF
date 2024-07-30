# 
# These two functions together allow greater control of propagating flags within
# a target on a per-source basis with the ability to "inherit" those properties 
# from the target if not set. This allows a target to defing its own flags, but
# then if a file needs different settings those can be directly overridden without
# relying on compiler-specific flag order precedence. Additionally this allows a
# project to organize grouping of flags within a target
#


#
# A simple function to create properties for targets and sources quickly
#
function( define_target_source_properties )
  set( options        )
  set( oneValueArgs   )
  set( multiValueArgs PROPERTIES )

  cmake_parse_arguments(
                        FUNC_PROP
                        "${options}"  "${oneValueArgs}"  "${multiValueArgs}"
                        ${ARGN}
                        )

  foreach( PROPERTY ${FUNC_PROP_PROPERTIES} )
    define_property(
                    SOURCE
                    PROPERTY   ${PROPERTY}
                    # INHERITED # they will be "inherited" via target to source
                    )

    define_property(
                    TARGET
                    PROPERTY   ${PROPERTY}
                    # INHERITED # they will be "inherited" via target to source
                    )
  endforeach()
endfunction()


#
# The bulk of the functionality exists in this function. It will loop over each
# provided target, gathering sources and their respective properties listed, using
# the target's property if not defined for this source else nothing, and finally 
# applies it to that source.
#
function( apply_target_source_properties )
  set( options        )
  set( oneValueArgs   AS_PROPERTY )
  set( multiValueArgs TARGETS PROPERTIES )

  cmake_parse_arguments(
                        FUNC_PROP
                        "${options}"  "${oneValueArgs}"  "${multiValueArgs}"
                        ${ARGN}
                        )

  foreach( TARGET ${FUNC_PROP_TARGETS} )
    # get target sources
    get_target_property( TARGET_SOURCES ${TARGET} SOURCES )

    # get default "inherited" value from target
    foreach( PROPERTY ${FUNC_PROP_PROPERTIES} )
      get_target_property( TARGET_PROPERTY_${PROPERTY} ${TARGET} ${PROPERTY} )
      if ( "${TARGET_PROPERTY_${PROPERTY}}" STREQUAL "TARGET_PROPERTY_${PROPERTY}-NOTFOUND" )
        # unset it
        set( TARGET_PROPERTY_${PROPERTY} )
      endif()
    endforeach()

    foreach( SOURCE ${TARGET_SOURCES} )

      # We need to accumulate properties since a call to set property will 
      # override what was there before
      set( SOURCE_PROPERTY_ALL )

      foreach( PROPERTY ${FUNC_PROP_PROPERTIES} )
        # first try source
        get_source_file_property( 
                                  SOURCE_PROPERTY_${PROPERTY}
                                  ${SOURCE}
                                  TARGET_DIRECTORY ${TARGET}
                                  ${PROPERTY}
                                  )
        if ( "${SOURCE_PROPERTY_${PROPERTY}}" STREQUAL "NOTFOUND" )
          # use target
          set( SOURCE_PROPERTY_${PROPERTY} ${TARGET_PROPERTY_${PROPERTY}} )
        endif()

        # Now apply these as prop
        if ( NOT "${SOURCE_PROPERTY_${PROPERTY}}" STREQUAL "" )
          list( APPEND SOURCE_PROPERTY_ALL ${SOURCE_PROPERTY_${PROPERTY}} )
        endif()
      endforeach() # properties

      # Apply properties to source
      if ( NOT "${SOURCE_PROPERTY_ALL}" STREQUAL "" )
        set_source_files_properties( 
                                        ${SOURCE}
                                        TARGET_DIRECTORY ${TARGET}
                                        PROPERTIES
                                          ${FUNC_PROP_AS_PROPERTY} ${SOURCE_PROPERTY_ALL}
                                        )
      endif()
    endforeach()   # sources
  endforeach()     # targets
endfunction()