# Copy the xmlreader input file to the build directory
execute_process(
   COMMAND ${CMAKE_COMMAND}
   -E copy ${CMAKE_CURRENT_SOURCE_DIR}/xmlreader_input.inp
           ${CMAKE_CURRENT_BINARY_DIR}/xmlreader.inp
   )

# Copy the xml template file to the build directory
execute_process(
   COMMAND ${CMAKE_COMMAND}
   -E copy ${CMAKE_CURRENT_SOURCE_DIR}/XMLtemplates/input.xml
           ${CMAKE_CURRENT_BINARY_DIR}
   )

# Run the xmlreader program to generate source code
execute_process(
   COMMAND ${XML-FORTRAN_BIN_DIR}/${XML-FORTRAN_EXECUTABLE}
   WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
   )

set(IO_SRC_FILES_INIT
   ${CMAKE_CURRENT_BINARY_DIR}/input.f90
   CACHE INTERNAL "" FORCE)

set(IO_SRC_FILES ${IO_SRC_FILES_INIT}
   ${CMAKE_CURRENT_SOURCE_DIR}/io.F90 
   CACHE INTERNAL "" FORCE)
