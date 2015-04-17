set(BASE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(BASE_SRC_FILES ${BASE_DIR}/xmlparse.f90
      ${BASE_DIR}/read_xml_prims.f90
      ${BASE_DIR}/write_xml_prims.f90
   CACHE INTERNAL "")
#${BASE_DIR}/xmlreader.F90
#add_executable(xmlreader ${BASE_DIR}/xmlreader.F90)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/xmlreader.F90.in
               ${CMAKE_CURRENT_BINARY_DIR}/xmlreader.F90 @ONLY)

set(XML_READER_EXECUTABLE
   ${CMAKE_CURRENT_BINARY_DIR}/xmlreader.F90
    CACHE INTERNAL "")
