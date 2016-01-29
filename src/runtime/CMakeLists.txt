cmake_minimum_required (VERSION 2.6)

set (CMAKE_C_FLAGS "-O2 -fno-omit-frame-pointer")
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/vsprof.ld.in ${CMAKE_CURRENT_BINARY_DIR}/vsprof.ld)
add_library (vsprof SHARED vsprof.c profiler_util.c samples.c
             samples.h)

target_link_libraries (vsprof pthread)

set_target_properties (vsprof PROPERTIES VERSION ${vsprofiler_VERSION}
                                         SOVERSION ${vsprofiler_VERSION_MAJOR}
                                         LINK_DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/vsprof.ld
                                         LINK_FLAGS "-Wl,--version-script ${CMAKE_CURRENT_BINARY_DIR}/vsprof.ld")

install (FILES vsprof.h DESTINATION include)
install (TARGETS vsprof LIBRARY
         DESTINATION lib)
