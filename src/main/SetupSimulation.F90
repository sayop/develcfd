!> \file: SetupSimulation.F90
!> \author: Sayop Kim

MODULE SetupSimulation_m
   USE Parameters_m, ONLY: wp

CONTAINS

!-----------------------------------------------------------------------------!
   SUBROUTINE InitializeMultiBlock(blk, nblk, ngc)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock
      USE InitMultiBlock_m

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: blk
      INTEGER, INTENT(IN) :: nblk, ngc
      INTEGER :: iblk

      ALLOCATE(blk(nblk))

      !> Read NODE files
      CALL ReadNODEfiles(blk, nblk, ngc)

      !> Read from grid file in plot3d file format
      !> Each block is simulataneously intialized and the variables are
      !> allocated while reading the grid data.
      CALL ReadStructuredGrid(blk, nblk, ngc)

      !> Read bc info for every surfaces surrouding the block
      !> Will read from bcinfo.dat
      !CALL ReadBCinfo(nblk, blk)

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE InitializeFlowVars(blk, nblk, ngc)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock
      USE FlowVariables_m, ONLY: FlowVars, AllocateConservedVars, &
                                 AllocatePrimitiveVars
      USE CommunicateData_m

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: nblk, ngc
      INTEGER :: iblk, nvar

      !> Number of conserved variables
      nvar = 5

      DO iblk = 1, nblk
         !> Allocate Q array: Conserved variables
         CALL AllocateConservedVars(blk(iblk)%flow, nvar, blk(iblk)%imin, &
                                                          blk(iblk)%imax, &
                                                          blk(iblk)%jmin, &
                                                          blk(iblk)%jmax, &
                                                          blk(iblk)%kmin, &
                                                          blk(iblk)%kmax)
         !>Allocate primitive variables: u,v,w,P,T,density, and etc.
         CALL AllocatePrimitiveVars(blk(iblk)%flow, blk(iblk)%imin, &
                                                    blk(iblk)%imax, &
                                                    blk(iblk)%jmin, &
                                                    blk(iblk)%jmax, &
                                                    blk(iblk)%kmin, &
                                                    blk(iblk)%kmax)
      END DO


      !> Initialize the solutions of flow variables
      CALL SetInitialConditions(blk, nblk)

      !> Receive node data from neighbors
      CALL RecvDataFromNeighbor(blk, nblk, ngc)

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE SetInitialConditions(blk, nblk)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock
      USE xml_data_input

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: nblk
      INTEGER :: iblk

      DO iblk = 1, nblk
         blk(iblk)%flow%RHO = 0.1_wp
         blk(iblk)%flow%U = 10.0_wp
         blk(iblk)%flow%V = 0.0_wp
         blk(iblk)%flow%W = 0.0_wp
         blk(iblk)%flow%T(blk(iblk)%istart:blk(iblk)%iend, &
                        blk(iblk)%jstart:blk(iblk)%jend, &
                        blk(iblk)%kstart:blk(iblk)%kend) = 297.0_wp
         blk(iblk)%flow%P = 101325.0_wp
      END DO

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE InitializeSimulationVars(blk, nblk, ngc)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock
      USE CoordinateTransform_m, ONLY: SetGridMetrics

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: nblk, ngc
      INTEGER :: iblk

      CALL SetGridMetrics(blk, nblk, 2)      

   END SUBROUTINE

END MODULE 
