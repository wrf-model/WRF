# WRF Macro to identify the commit where the compiled code came from
macro( wrf_git_commit )

  set( options        )
  set( oneValueArgs   WORKING_DIRECTORY RESULT_VAR )
  set( multiValueArgs )

  cmake_parse_arguments(
                        WRF_GIT_COMMIT
                        "${options}"  "${oneValueArgs}"  "${multiValueArgs}"
                        ${ARGN}
                        )
  
  
  message( STATUS "Retrieving git information..." )
  execute_process(
                  OUTPUT_VARIABLE       WRF_GIT_COMMIT_SHA
                  COMMAND               git describe --dirty --long --always --abbrev=40
                  WORKING_DIRECTORY     ${WRF_GIT_COMMIT_WORKING_DIRECTORY}
                  RESULT_VARIABLE       WRF_GIT_COMMIT_NO_GIT_REPO
                  # ECHO_OUTPUT_VARIABLE
                  OUTPUT_STRIP_TRAILING_WHITESPACE
                  )
  execute_process(
                  OUTPUT_VARIABLE       WRF_GIT_COMMIT_DIFF
                  COMMAND               git diff --shortstat
                  WORKING_DIRECTORY     ${WRF_GIT_COMMIT_WORKING_DIRECTORY}
                  # ECHO_OUTPUT_VARIABLE
                  OUTPUT_STRIP_TRAILING_WHITESPACE
                  )

  if ( ${WRF_GIT_COMMIT_NO_GIT_REPO} GREATER 0 )
    set( ${WRF_GIT_COMMIT_RESULT_VAR} "No git found or not a git repository, git commit version not available.")
    message( STATUS "git info : Unable to get info" )
  else()
    set( ${WRF_GIT_COMMIT_RESULT_VAR} "git info : ${WRF_GIT_COMMIT_SHA} ${WRF_GIT_COMMIT_DIFF}" )
    message( STATUS "git SHA  : ${WRF_GIT_COMMIT_SHA}" )
    message( STATUS "git diff : ${WRF_GIT_COMMIT_DIFF}" )
  endif()
  
endmacro()