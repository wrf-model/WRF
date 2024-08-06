# Port of cflags.guess logic
# If the compiler starts with "cc"
if ( ${CMAKE_C_COMPILER} MATCHES "(/|\\\\)cc[A-Za-z0-9_]*$" )
  if ( ${CMAKE_SYSTEM_NAME} STREQUAL "SunOS" )
    message( FATAL_ERROR "KPP must be compiled with gcc on SunOS machines" )
  endif()

  set( 
      KPP_CC_FLAGS
      "$<$<AND:$<PLATFORM_ID:HP-UX>,$<COMPILE_LANGUAGE:C>>:-Aa;-D_HPUX_SOURCE>" #@ Hewlett Packard Unix
      "$<$<AND:$<PLATFORM_ID:AIX>,$<COMPILE_LANGUAGE:C>>:-Aa>"                  #@ IBM Unix operating system
      )
endif()


FLEX_TARGET(
            kpp_lex
            src/scan.l
            ${CMAKE_CURRENT_BINARY_DIR}/lex.yy.c
            )

BISON_TARGET(
              kpp_tab
              src/scan.y
              ${CMAKE_CURRENT_BINARY_DIR}/y.tab.c
              )

add_executable(
                kpp
                ${FLEX_kpp_lex_OUTPUTS}
                ${BISON_kpp_tab_OUTPUTS}
                src/scanner.c
                src/scanutil.c
                src/kpp.c
                src/gen.c
                src/code.c
                src/code_c.c
                src/code_f77.c
                src/code_f90.c
                src/code_matlab.c
                src/debug.c
                )

target_include_directories(
                            kpp
                            PRIVATE
                              ${CMAKE_CURRENT_SOURCE_DIR}/src
                              ${CMAKE_CURRENT_BINARY_DIR}
                            )
target_link_libraries( kpp PRIVATE ${FLEX_LIBRARIES} )
