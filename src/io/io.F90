!> \file: io.F90
!> \author: Sayop Kim
!> \brief: Provides routines to read input and write output

MODULE io_m
   USE Parameters_m, ONLY: wp

   REAL(KIND=wp) :: rhoinit, uinit, vinit, cgammainit, pinit
   INTEGER, PARAMETER :: IOunit = 10, filenameLength = 64
   CHARACTER(LEN=50) :: prjTitle
   CHARACTER(LEN=filenameLength) :: gridFile
CONTAINS

!-----------------------------------------------------------------------------!
   SUBROUTINE ReadInput()
!-----------------------------------------------------------------------------!

     IMPLICIT NONE
     INTEGER :: ios

     OPEN(IOunit, FILE = 'input.dat', FORM = 'FORMATTED', ACTION = 'READ', &
           STATUS = 'OLD', IOSTAT = ios)
     IF(ios /= 0) THEN
        WRITE(*,'(a)') ""
        WRITE(*,'(a)') "Fatal error: Could not open the input data file."
        RETURN
     ELSE
        WRITE(*,'(a)') ""
        WRITE(*,'(a)') "Reading input file for transformation 1"
     ENDIF




   END SUBROUTINE ReadInput

END MODULE io_m
