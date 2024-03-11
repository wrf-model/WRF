# Compile once and only ONCE
add_executable( integration_edit integr_edit.c )
target_compile_definitions( integration_edit PRIVATE -DNO_COPY )
