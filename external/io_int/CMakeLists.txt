# WRF CMake Build
# get_filename_component( FOLDER_COMPILE_TARGET ${CMAKE_CURRENT_SOURCE_DIR} NAME)

# add_library(
#             ${FOLDER_COMPILE_TARGET}
#               STATIC
#               )

# target_sources(
#                 ${FOLDER_COMPILE_TARGET}
#                 PRIVATE
target_sources(
                ${PROJECT_NAME}_Core
                PRIVATE
                  io_int.F90
                  io_int_idx.c
                  module_io_int_idx.F90
                  module_io_int_read.F90
                  )

# set_target_properties( 
#                       ${FOLDER_COMPILE_TARGET} 
#                         PROPERTIES
#                           Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${FOLDER_COMPILE_TARGET}
#                           Fortran_FORMAT           FREE                          
#                       )


# target_link_libraries(  ${FOLDER_COMPILE_TARGET}
#                           PRIVATE
#                             ${netCDF_LIBRARIES}
#                             $<$<BOOL:${USE_MPI}>:$<TARGET_NAME_IF_EXISTS:MPI::MPI_Fortran>>
#                             $<$<BOOL:${USE_OPENMP}>:$<TARGET_NAME_IF_EXISTS:OpenMP::OpenMP_Fortran>>
#                         )

# target_include_directories( ${FOLDER_COMPILE_TARGET}
#                             PRIVATE
#                               ${netCDF_INCLUDE_DIRS}
#                               ${CMAKE_CURRENT_SOURCE_DIR}
#                               #!TODO Fix duplicates of wrf_[io|status]_flags.h
#                               # ${CMAKE_CURRENT_SOURCE_DIR}/../ioapi_share 
#                               ${CMAKE_CURRENT_SOURCE_DIR}/../io_grib_share
#                               ${CMAKE_CURRENT_SOURCE_DIR}/../../inc
#                             )

# install(
#         TARGETS ${FOLDER_COMPILE_TARGET}
#         RUNTIME DESTINATION bin/
#         ARCHIVE DESTINATION lib/
#         LIBRARY DESTINATION lib/
#         )
