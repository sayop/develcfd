cmake_minimum_required(VERSION 2.6)

project(PreProcessor)

enable_language(Fortran)

find_package(xml-fortran)
set(CFD_BASE_DIR ${PROJECT_SOURCE_DIR}/../../src/ CACHE INTERNAL " ")
include_directories(${XML-FORTRAN_INCLUDE_DIRS})
link_directories(${XML-FORTRAN_LIBRARY_DIRS})

#
# add sub-directories defined for each certain purpose
#
#add_subdirectory(${CFD_BASE_DIR}/main
#     ${CMAKE_CURRENT_BINARY_DIR}/main)
add_subdirectory(${CFD_BASE_DIR}/io
     ${CMAKE_CURRENT_BINARY_DIR}/io)

#
# set executable file name
#
set(PRE_EXE_NAME pre.x CACHE STRING "PRE executable name")

#
# set source files
#
set(PRE_SRC_FILES
   ${CFD_BASE_DIR}/main/Parameters.F90
   ${CFD_BASE_DIR}/variables/MultiDomainVars.F90
   ${PREIO_SRC_FILES}
   ${CMAKE_CURRENT_SOURCE_DIR}/PreSetup.F90
   ${CMAKE_CURRENT_SOURCE_DIR}/pre.F90 CACHE INTERNAL "")
                  
#
# define executable
#
add_executable(${PRE_EXE_NAME} ${PRE_SRC_FILES})
target_link_libraries(${PRE_EXE_NAME} xmlparse)
