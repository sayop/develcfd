!> \file: main.F90
!> \author: Sayop Kim

PROGRAM main
   USE Parameters_m, ONLY: wp
   USE io_m

   IMPLICIT NONE

   WRITE(*,*) 'SAYOP'
   !CALL ReadInputFiles()
   CALL WriteInputFiles()

END PROGRAM main
