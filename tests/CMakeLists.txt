cmake_minimum_required (VERSION 2.6)

add_library (ex SHARED EXCLUDE_FROM_ALL libexample.c)
add_executable (example EXCLUDE_FROM_ALL example.c)

target_link_libraries (example ex)

add_custom_target (runtime-check
                   DEPENDS vsprof example
                   COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/run-tests.sh)
