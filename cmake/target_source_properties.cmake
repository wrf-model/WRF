# 
# These two functions together allow greater control of propagating flags within
# a target on a per-source basis with the ability to "inherit" those properties 
# from the target if not set. This allows a target to defing its own flags, but
# then if a file needs different settings those can be directly overridden without
# relying on compiler-specific flag order precedence. Additionally this allows a
# project to organize grouping of flags within a target
#
# Note that for compile defines on source files they are not used in the autogen
# dependency scanning. See :
# https://gitlab.kitware.com/cmake/cmake/-/issues/22519
# and 
# https://gitlab.kitware.com/cmake/cmake/-/blob/master/Source/cmDependsFortran.cxx#L84
# functions cmDependsFortran::cmDependsFortran() and cmDependsFortran::WriteDependencies()
# 
# The solution is to either use Ninja or preprocess the files (what Ninja internally does)
# This is probably the way to go as well since CMake native preprocessor directive
# parsing is... subpar and simplified :
# https://gitlab.kitware.com/cmake/cmake/-/issues/17398
# 
# Alternatively, set critical flags at the target level
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
  set( options        DISCARD_PREVIOUS DEBUG )
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
          if ( ${FUNC_PROP_DEBUG} )
            message( STATUS "DEBUG : Adding '${SOURCE_PROPERTY_${PROPERTY}}' as SOURCE_PROPERTY_${PROPERTY}")
          endif()
          list( APPEND SOURCE_PROPERTY_ALL ${SOURCE_PROPERTY_${PROPERTY}} )
        endif()
      endforeach() # properties

      # Apply properties to source
      if ( NOT "${SOURCE_PROPERTY_ALL}" STREQUAL "" )
        if ( NOT ${FUNC_PROP_DISCARD_PREVIOUS} )
          # get old value and append
          get_source_file_property(
                                    SOURCE_PROPERTY_ORIG
                                    ${SOURCE}
                                    TARGET_DIRECTORY ${TARGET}
                                    ${FUNC_PROP_AS_PROPERTY}
                                    )
          if ( "${SOURCE_PROPERTY_ORIG}" STREQUAL "NOTFOUND" )
            set( SOURCE_PROPERTY_ORIG )
          endif()
        endif()

        if ( ${FUNC_PROP_DEBUG} )
          message( STATUS "DEBUG : ${FUNC_PROP_AS_PROPERTY} being set to '${SOURCE_PROPERTY_ORIG} ${SOURCE_PROPERTY_ALL}'")
        endif()

        set_source_files_properties(
                                    ${SOURCE}
                                    TARGET_DIRECTORY ${TARGET}
                                    PROPERTIES
                                      ${FUNC_PROP_AS_PROPERTY} "${SOURCE_PROPERTY_ORIG};${SOURCE_PROPERTY_ALL}"
                                    )
      endif()
    endforeach()   # sources
  endforeach()     # targets
endfunction()
