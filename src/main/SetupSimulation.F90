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
      CALL ReadBCinfo(nblk, blk)

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE InitializeFlowVars(blk, nblk)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock
      USE FlowVariables_m, ONLY: FlowVars, AllocateConservedVars, &
                                 AllocatePrimitiveVars
      USE CommunicateData_m

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: nblk
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

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE InitializeSpeciesVars(blk, nblk)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock
      USE SpeciesVars_m
      USE xml_data_input

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: nblk
      INTEGER :: iblk, ispc

      nspec = input_data%Species%nspec
      !> Allocate memories for global species data
      ALLOCATE(SPC(nspec))

      DO ispc = 1, nspec
         SPC(ispc)%spcName  = input_data%Species%species(ispc)
         SPC(ispc)%Lewis    = input_data%Species%lewis(ispc)
         !> Temporary setup for specific heat: Here AIR property is adopted!
         SPC(ispc)%Cp       = 1010.0_wp
         SPC(ispc)%Cv       = 718.0_wp
         SPC(ispc)%R        = SPC(ispc)%Cp - SPC(ispc)%Cv
      END DO
      
      DO iblk = 1, nblk
         CALL AllocateSpeciesData(blk(iblk)%spc, nspec, blk(iblk)%imin, &
                                                        blk(iblk)%imax, &
                                                        blk(iblk)%jmin, &
                                                        blk(iblk)%jmax, &
                                                        blk(iblk)%kmin, &
                                                        blk(iblk)%kmax)
      END DO

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE SetInitialConditions(blk, nblk)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock
      USE xml_data_input
      USE PerfectGas_m, ONLY: EvaluateDensity

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: nblk
      INTEGER :: iblk
      REAL(KIND=wp) :: Rmix

      !> Temporary assignment.
      Rmix = 287.05_wp

      DO iblk = 1, nblk
         blk(iblk)%flow%RHO = EvaluateDensity(input_data%InitialCondition%pres, &
                                              input_data%Initialcondition%temp, &
                                              Rmix)
         blk(iblk)%flow%U   = input_data%InitialCondition%u
         blk(iblk)%flow%V   = input_data%InitialCondition%v
         blk(iblk)%flow%W   = input_data%InitialCondition%w
         blk(iblk)%flow%P   = input_data%InitialCondition%pres
         blk(iblk)%flow%T   = input_data%Initialcondition%temp
      END DO

   END SUBROUTINE


!-----------------------------------------------------------------------------!
   SUBROUTINE InitializeSimulationVars(blk, nblk)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock
      USE CoordinateTransform_m, ONLY: SetGridMetrics
      USE xml_data_input

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: nblk
      INTEGER :: iblk, norder

      !> order of accuracy for spatial discretization in generalized transform
      norder = input_data%GridTransform%order

      CALL SetGridMetrics(blk, nblk, norder)

   END SUBROUTINE

END MODULE 
