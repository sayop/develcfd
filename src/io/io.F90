!> \file: io.F90
!> \author: Sayop Kim
!> \brief: Provides routines to read input and write output

MODULE io_m
   USE Parameters_m, ONLY: wp
   USE xml_data_input

   REAL(KIND=wp) :: rhoinit, uinit, vinit, cgammainit, pinit
   INTEGER, PARAMETER :: IOunit = 10, filenameLength = 64
   CHARACTER(LEN=50) :: prjTitle
CONTAINS

!-----------------------------------------------------------------------------!
   SUBROUTINE ReadInputFiles()
!-----------------------------------------------------------------------------!

     IMPLICIT NONE
     CHARACTER(LEN=filenameLength) :: filename

     filename = 'cfd_input.xml'

     CALL read_xml_file_input(filename)


   END SUBROUTINE ReadInputFiles

!-----------------------------------------------------------------------------!
   SUBROUTINE WriteInputFiles()
!-----------------------------------------------------------------------------!

     IMPLICIT NONE
     CHARACTER(LEN=filenameLength) :: filename

     filename = 'cfd_input.xml'
     CALL write_xml_file_input(filename)


   END SUBROUTINE WriteInputFiles

END MODULE io_m
