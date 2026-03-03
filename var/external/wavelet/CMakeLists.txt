# WRF CMake Build
get_filename_component( FOLDER_COMPILE_TARGET ${CMAKE_CURRENT_SOURCE_DIR} NAME )

add_library(
            ${FOLDER_COMPILE_TARGET} # C library
              STATIC
                awaprogs/chap05/cd.c
                dwtai.c
                dwtai_w.c
                dwtai2.c
                dwtai2_w.c
                DWTAPartition.c
                idwtai.c
                idwtai_w.c
                idwtai2.c
                idwtai2_w.c
                l2norm.c
                PrintFilter.c
                awaprogs/chap05/qf.c
                qf_w.c
                qftest.c
                qftest_w.c
                TestFilter.c
                awaprogs/chap09/xp.c
              )


add_library(
            ${FOLDER_COMPILE_TARGET}f # Fortran library
              STATIC
                dwta_partition.F90
                print_filter.F90
                test_filter.F90
              )


target_include_directories(
                            ${FOLDER_COMPILE_TARGET}
                            PRIVATE
                              ${CMAKE_CURRENT_SOURCE_DIR}
                              ${CMAKE_CURRENT_SOURCE_DIR}/awaprogs/chap02
                              ${CMAKE_CURRENT_SOURCE_DIR}/awaprogs/chap05
                            )

# This is new in the cmake build
target_compile_options(
                        ${FOLDER_COMPILE_TARGET}
                        PRIVATE
                          ${PROJECT_COMPILE_OPTIONS_OPTIMIZATION}
                        )

target_compile_definitions(
                            ${FOLDER_COMPILE_TARGET}
                            PRIVATE
                              ${PROJECT_COMPILE_DEFINITIONS_CONFCHECK}
                            )

install(
        TARGETS ${FOLDER_COMPILE_TARGET} ${FOLDER_COMPILE_TARGET}f
        EXPORT  ${EXPORT_NAME}Targets
        RUNTIME DESTINATION bin/
        ARCHIVE DESTINATION lib/
        LIBRARY DESTINATION lib/
        )
