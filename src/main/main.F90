!> \file: main.F90
!> \author: Sayop Kim

PROGRAM main
   USE Parameters_m, ONLY: wp
   USE MultiBlockVars_m
   USE io_m
   USE SetupSimulation_m

   IMPLICIT NONE
   TYPE(MultiBlock), DIMENSION(:), ALLOCATABLE, TARGET :: blk
   REAL(KIND=wp) :: START, FINISH
   CALL CPU_TIME(START)

   WRITE(*,*) "##########################################################################"
   WRITE(*,*) "##########################################################################"
   WRITE(*,*) "### COMPUTATIONAL FLUID DYNAMICS: 3D COMPRESSIBLE FLOW SIMULATION CODE ###"
   WRITE(*,*) "##########################################################################"
   WRITE(*,*) "##########################################################################"

   CALL ReadInputFiles()
   CALL WriteInputFiles()

   !> Initialize global variables for multiblock setup
   ndomain = input_data%MultiBlock%ndomain
   nblk    = input_data%MultiBlock%nblk
   ngc     = input_data%MultiBlock%ngc
   ngls    = input_data%MultiBlock%ngls

   !> Read grid file and allocate multiblock variables
   CALL InitializeMultiBlock(blk, nblk, ngc)

   CALL InitializeSimulationVars(blk, nblk)

   CALL InitializeFlowVars(blk, nblk)

   CALL InitializeSpeciesVars(blk, nblk)

   CALL SetInitialConditions(blk, nblk)

   CALL WriteRESTfile(blk, nblk)

   CALL CPU_TIME(FINISH)
   WRITE(*,*) "TOTAL CPU TIME: ", FINISH-START, " SECONDS"
END PROGRAM main
