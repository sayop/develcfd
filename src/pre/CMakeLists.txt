cmake_minimum_required(VERSION 2.6)

project(PREPROCESS)

enable_language(Fortran)

find_package(xml-fortran)
include_directories(${XML-FORTRAN_INCLUDE_DIRS})
link_directories(${XML-FORTRAN_LIBRARY_DIRS})

#
# set executable file name
#
set(PRE_EXE_NAME pre.x CACHE STRING "PREPROCESS executable name")

#
# set source files
#
set(PRE_SRC_FILES
   ${CMAKE_CURRENT_SOURCE_DIR}/pre.F90
   CACHE INTERNAL "" FORCE)

#
# define executable
#
add_executable(${PRE_EXE_NAME} ${PRE_SRC_FILES})
target_link_libraries(${PRE_EXE_NAME} xmlparse)
