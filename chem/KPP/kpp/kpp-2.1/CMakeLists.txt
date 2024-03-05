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
            scan.l
            ${CMAKE_CURRENT_BINARY_DIR}/preproc/lex.yy.c
            )

BISON_TARGET(
              kpp_tab
              scan.y
              ${CMAKE_CURRENT_BINARY_DIR}/preproc/y.tab.c
              )

add_executable(
                kpp
                ${FLEX_kpp_lex_OUTPUTS}
                ${BISON_kpp_tab_OUTPUTS}
                scanner.c
                scanutil.c
                kpp.c
                gen.c
                code.c
                code_c.c
                code_f77.c
                code_f90.c
                code_matlab.c
                debug.c
                )

target_link_libraries( kpp PRIVATE ${FLEX_LIBRARIES} )