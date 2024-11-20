# https://stackoverflow.com/a/19578320
# Some color defintions
if ( NOT "${PRINT_OPTION_SUPPRESS_COLOR}" )
  if ( NOT WIN32 )
    string( ASCII 27 ESC )
    set( COLOR_RESET  "${ESC}[m"     )
    set( COLOR_BOLD   "${ESC}[1m"    )
    set( RED          "${ESC}[31m"   )
    set( GREEN        "${ESC}[32m"   )
    set( YELLOW       "${ESC}[33m"   )
    set( BLUE         "${ESC}[34m"   )
    set( MAGENTA      "${ESC}[35m"   )
    set( CYAN         "${ESC}[36m"   )
    set( WHITE        "${ESC}[37m"   )
    set( BOLD_RED      "${ESC}[1;31m" )
    set( BOLD_GREEN    "${ESC}[1;32m" )
    set( BOLD_YELLOW   "${ESC}[1;33m" )
    set( BOLD_BLUE     "${ESC}[1;34m" )
    set( BOLD_MAGENTA  "${ESC}[1;35m" )
    set( BOLD_CYAN     "${ESC}[1;36m" )
    set( BOLD_WHITE    "${ESC}[1;37m" )
  endif()
endif()

function( print_option )
  set( OPTION   ${ARGV0} )
  set( JUSTIFY  ${ARGV1} )

  if ( ${ARGC} GREATER_EQUAL 3 )
    set( ALT_COLOR ${ARGV2} )
  endif()

  if ( DEFINED ALT_COLOR )
    set( OPT_COLOR ${ALT_COLOR} )
  else()
    set( OPT_COLOR ${RED} )
    if ( ${${OPTION}} )
      set( OPT_COLOR ${GREEN} )
    endif()
  endif()
  
  set( OPTION_STR "${OPTION}" )
  string( LENGTH ${OPTION_STR} OPTION_STR_LEN )
  math( EXPR N_JUSTIFY "${JUSTIFY} - ${OPTION_STR_LEN}" )

  if ( ${N_JUSTIFY} LESS 1 )
    set( N_JUSTIFY 1 )
  endif()

  string( REPEAT " " ${N_JUSTIFY} JUSTIFY_WHITESPACE )

  message( STATUS "${OPTION_STR}${JUSTIFY_WHITESPACE} : ${OPT_COLOR}${${OPTION}}${COLOR_RESET}" )

endfunction()