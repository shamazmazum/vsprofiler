cmake_minimum_required (VERSION 2.6)

set (LISP_IMPL sbcl --script CACHE STRING "Lisp implementation")

add_custom_target (analizer ALL
                   DEPENDS vsanalizer)
add_custom_command (OUTPUT vsanalizer.bin
                    COMMAND sh os-name.sh
                    COMMAND ${LISP_IMPL} build.lisp
                    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
add_custom_command (OUTPUT vsanalizer
                    DEPENDS vsanalizer.bin
                    COMMAND mv ${CMAKE_CURRENT_SOURCE_DIR}/vsanalizer.bin vsanalizer
                    COMMAND rm ${CMAKE_CURRENT_SOURCE_DIR}/os-name.lisp-expr)

#install (PROGRAMS analizer
#         DESTINATION bin)
