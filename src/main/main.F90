!> \file: main.F90
!> \author: Sayop Kim

PROGRAM main
   USE Parameters_m, ONLY: wp
   USE MultiBlockVars_m, ONLY: MultiBlock
   USE GlobalVars_m
   USE io_m
   USE SetupSimulation_m

   IMPLICIT NONE

   TYPE(MultiBlock), DIMENSION(:), ALLOCATABLE, TARGET :: blk
   REAL(KIND=wp) :: START, FINISH

   CALL CPU_TIME(START)

   !> Read xml input file and initialize basic block related variables: ndomain, nblk, 
   !>                                                                   ngc, ngls, ncpu
   CALL ReadInputFiles()
   !> Write new_input.xml for back-up
   CALL WriteInputFiles()

#ifndef SERIAL
   !> Initialize block distribution info: Read BLK_DIST.DATA
   CALL InitializeParallelComputing(ncpu)
#else
   !> If it runs in serial mode, only a single CPU will cover entire blocks
   nbp = nblk
#endif

   !> Read grid file and allocate multiblock variables
   CALL InitializeMultiBlock(blk, nbp, ngc)

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
