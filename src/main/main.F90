!> \file: main.F90
!> \author: Sayop Kim

PROGRAM main
   USE Parameters_m, ONLY: wp
   USE MultiBlockVars_m, ONLY: MultiBlock, nbp
   USE GlobalVars_m
   USE SetupSimulation_m

   IMPLICIT NONE

   TYPE(MultiBlock), DIMENSION(:), ALLOCATABLE, TARGET :: blk
   REAL(KIND=wp) :: START, FINISH

   CALL CPU_TIME(START)

   CALL InitializeSimulation()
  

   !> Read grid file and allocate multiblock variables
!   CALL InitializeMultiBlock(blk, nbp, ngc)

!   CALL InitializeSimulationVars(blk, nblk)

!   CALL InitializeFlowVars(blk, nblk)

!   CALL InitializeSpeciesVars(blk, nblk)

!   CALL SetInitialConditions(blk, nblk)

!   CALL WriteRESTfile(blk, nblk)

#ifndef SERIAL
   CALL FinalizeParallelComputing()
#endif
   IF (rank .EQ. 0) THEN
      CALL CPU_TIME(FINISH)
      WRITE(*,*) "TOTAL CPU TIME: ", FINISH-START, " SECONDS"
   END IF

END PROGRAM main
