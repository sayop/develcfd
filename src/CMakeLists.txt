cmake_minimum_required(VERSION 2.6)

project(CFD)

enable_language(Fortran)

#
# set executable file name
#
set(EXE_NAME cfd.x CACHE STRING "CFD executable name")

#
# Find xml-fortran package
#
find_package(xml-fortran
             HINTS $ENV{XML_FORTRAN_ROOT})
include_directories(${XML-FORTRAN_INCLUDE_DIRS})
link_directories(${XML-FORTRAN_LIBRARY_DIRS})

#
# add sub-directories defined for each certain purpose
#
add_subdirectory(main)
add_subdirectory(io)
add_subdirectory(multiblock)
add_subdirectory(variables)
add_subdirectory(properties)
add_subdirectory(FDM)


#
# set source files
#
set(CFD_SRC_FILES ${MAIN_SRC_FILES}
                  ${IO_SRC_FILES}
                  ${VARS_SRC_FILES}
                  ${PROPERTY_SRC_FILES}
                  ${FDM_SRC_FILES}
                  ${MULTIBLOCK_SRC_FILES})
                  
#
# define executable
#
add_executable(${EXE_NAME} ${CFD_SRC_FILES})
target_link_libraries(${EXE_NAME} xmlparse)

#
# Enable MPI use 
#
option(MPI_USE "Enable parallel computing" ON)
#mark_as_advanced(MPI_USE)
if(NOT MPI_USE)
   add_definitions(-DSERIAL)
else()
   # Require MPI for this project
   find_package(MPI REQUIRED)
   add_definitions(${MPI_Fortran_COMPILE_FLAGS})
   include_directories(${MPI_Fortran_INCLUDE_PATH})
   link_directories(${MPI_Fortran_LIBRARIES})
   target_link_libraries(${EXE_NAME} ${MPI_Fortran_LIBRARIES})
endif()

