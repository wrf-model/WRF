# WRF CMake Build
get_filename_component( FOLDER_COMPILE_TARGET ${CMAKE_CURRENT_SOURCE_DIR} NAME)

add_library(
            ${FOLDER_COMPILE_TARGET}
              STATIC
              )
# Test1_ESMF
# Test1_WRFU
target_sources(
                ${FOLDER_COMPILE_TARGET}
                PRIVATE
                  ESMF_Alarm.F90
                  ESMF_BaseTime.F90
                  ESMF_Clock.F90
                  ESMF_Time.F90
                  Meat.F90
                  ESMF_Base.F90
                  ESMF_Calendar.F90
                  ESMF_Fraction.F90
                  ESMF_TimeInterval.F90
                  ESMF_Stubs.F90
                  ESMF_Mod.F90
                  module_symbols_util.F90
                  module_utility.F90
                  ESMF_AlarmClock.F90
                  )

# target_compile_options(
#                         ${FOLDER_COMPILE_TARGET}
#                         PRIVATE
#                           # Specific flags for this target
#                       )

set_target_properties( 
                      ${FOLDER_COMPILE_TARGET} 
                        PROPERTIES
                          Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${FOLDER_COMPILE_TARGET}
                          EXPORT_PROPERTIES        Fortran_MODULE_DIRECTORY
                      )


target_link_libraries(  ${FOLDER_COMPILE_TARGET}
                          PRIVATE
                            $<$<BOOL:${USE_MPI}>:$<TARGET_NAME_IF_EXISTS:MPI::MPI_Fortran>>
                            $<$<BOOL:${USE_OPENMP}>:$<TARGET_NAME_IF_EXISTS:OpenMP::OpenMP_Fortran>>
                          )

target_include_directories( ${FOLDER_COMPILE_TARGET}
                            PRIVATE
                              ${CMAKE_CURRENT_SOURCE_DIR}
                            )

install(
        TARGETS ${FOLDER_COMPILE_TARGET}
        EXPORT  ${EXPORT_NAME}Targets
        RUNTIME DESTINATION bin/
        ARCHIVE DESTINATION lib/
        LIBRARY DESTINATION lib/
        )
