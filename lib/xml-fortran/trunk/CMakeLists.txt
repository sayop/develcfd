cmake_minimum_required(VERSION 2.6)

project(xml-fortran)
enable_language(Fortran)

set(XML-FORTRAN_MAJOR_VERSION 1)
set(XML-FORTRAN_MINOR_VERSION 0)
set(XML-FORTRAN_PATCH_VERSION 0)
set(XML-FORTRAN_VERSION
     ${XML-FORTRAN_MAJOR_VERSION}.${XML-FORTRAN_MINOR_VERSION}.${XML-FORTRAN_PATCH_VERSION})

set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/Modules)

include(CMakeDependentOption)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING
      "Choose the type of build, options are: None Debug Release RelWithDebInfo MinSizeRel."
      FORCE)
endif(NOT CMAKE_BUILD_TYPE)

get_filename_component (default_prefix ${CMAKE_BINARY_DIR} ABSOLUTE)
if(NOT CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX ${default_prefix} CACHE STRING
      "Path to the installation directory" FORCE)
endif(NOT CMAKE_INSTALL_PREFIX)
if(CMAKE_INSTALL_PREFIX STREQUAL "/usr/local")
  set(CMAKE_INSTALL_PREFIX "${default_prefix}" CACHE STRING
      "Path to the installation directory" FORCE)
endif(CMAKE_INSTALL_PREFIX STREQUAL "/usr/local")

###############################################################################
# Compiler options
###############################################################################
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
if (Fortran_COMPILER_NAME STREQUAL "gfortran")
  # gfortran
  set (CMAKE_Fortran_FLAGS_RELEASE "-O3")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
elseif (Fortran_COMPILER_NAME STREQUAL "ifort")
  # ifort
  set (CMAKE_Fortran_FLAGS_RELEASE "-O3")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
elseif (Fortran_COMPILER_NAME STREQUAL "g77")
  # g77
  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3 -m32")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -m32")
else (Fortran_COMPILER_NAME STREQUAL "gfortran")
  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
  message ("No optimized Fortran compiler flags are known, we just try -O2...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
endif (Fortran_COMPILER_NAME STREQUAL "gfortran")

###############################################################################
set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_INSTALL_PREFIX}/include")
add_subdirectory(src)

set(LIB_SRC_FILES ${BASE_SRC_FILES})

add_library(xmlparse ${LIB_SRC_FILES})

add_executable(xmlreader ${XML_READER_EXECUTABLE})
target_link_libraries(xmlreader xmlparse)
###############################################################################
###############################################################################
#option(BUILD_EXAMPLES "Build examples showing how to use the library" ON)
##option(BUILD_SHARED_LIBS "Build shared libraries" OFF)
###############################################################################
###############################################################################
set(INSTALL_BIN_DIR "${CMAKE_INSTALL_PREFIX}/bin")
set(INSTALL_INCLUDE_DIR "${CMAKE_INSTALL_PREFIX}/include")
set(INSTALL_LIB_DIR "${CMAKE_INSTALL_PREFIX}/lib")
#set(XMLREADER_EXECUTABLE "${INSTALL_BIN_DIR}/xmlreader")

#file(GLOB modFiles "${CMAKE_CURRENT_BINARY_DIR}/*.mod")
#install(FILES ${modFiles} DESTINATION ${INSTALL_INCLUDE_DIR})

set(MAX_LINES 1000 CACHE STRING "Maximum number of lines that can be read per tag")

install(TARGETS xmlparse
        LIBRARY DESTINATION ${INSTALL_LIB_DIR}
  ARCHIVE DESTINATION ${INSTALL_LIB_DIR})

configure_file(xml-fortranConfig.cmake.in 
   "${CMAKE_CURRENT_BINARY_DIR}/xml-fortranConfig.cmake" @ONLY)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/xml-fortranConfig.cmake"
   DESTINATION ${CMAKE_INSTALL_PREFIX})

INCLUDE(CMakePackageConfigHelpers)
WRITE_BASIC_PACKAGE_VERSION_FILE(xml-fortranConfigVersion.cmake
   VERSION ${XML-FORTRAN_VERSION}
   COMPATIBILITY ExactVersion)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/xml-fortranConfigVersion.cmake"
   DESTINATION ${CMAKE_INSTALL_PREFIX})

install(TARGETS xmlreader
   EXPORT xml-fortranLibraryDependencies
   RUNTIME DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" COMPONENT bin)


