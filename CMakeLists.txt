cmake_minimum_required (VERSION 2.6)

project (vsprofiler)
set (vsprofiler_VERSION_MAJOR 1)
set (vsprofiler_VERSION_MINOR 0)
set (vsprofiler_VERSION ${vsprofiler_VERSION_MAJOR}.${vsprofiler_VERSION_MINOR})

add_subdirectory (src/runtime)
add_subdirectory (src/analizer)
add_subdirectory (tests)

add_custom_target (check
                  DEPENDS runtime-check
#                  DEPENDS vsanalizer
                  COMMAND ./src/analizer/vsanalizer test tests/prof1.smpl tests/prof1.map)
