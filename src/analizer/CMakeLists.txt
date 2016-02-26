cmake_minimum_required (VERSION 2.6)

set (LISP_IMPL sbcl --script CACHE STRING "Lisp implementation")
file (GLOB LISP_SOURCES RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.lisp)

foreach (SOURCE ${LISP_SOURCES})
    configure_file (${SOURCE} ${CMAKE_CURRENT_BINARY_DIR}/${SOURCE})
endforeach (SOURCE)
configure_file (os-name.sh ${CMAKE_CURRENT_BINARY_DIR}/os-name.sh)

add_custom_command (OUTPUT vsanalizer
                    COMMAND sh os-name.sh
                    COMMAND ${LISP_IMPL} build.lisp
                    DEPENDS ${LISP_SOURCES})
add_custom_target (analizer ALL
                  DEPENDS vsanalizer)

install (PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/vsanalizer
         DESTINATION bin)
