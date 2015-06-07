!> \file: main.F90
!> \author: Sayop Kim

PROGRAM main
   USE Parameters_m, ONLY: wp
   USE MultiBlockVars_m
   USE io_m
   USE InitMultiBlock_m

   IMPLICIT NONE
   TYPE(MultiBlock), DIMENSION(:), ALLOCATABLE, TARGET :: blk

   !WRITE(*,*) 'SAYOP'
   CALL ReadInputFiles()
   !> Read grid file and allocate multiblock variables
   CALL InitializeMultiBlock(blk)
   !CALL WriteInputFiles()

END PROGRAM main
