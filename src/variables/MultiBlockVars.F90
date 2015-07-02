!> \file: MultiBlockVars.F90
!> \author: Sayop Kim
!> \brief: Provides variables related multiblocks grid data

MODULE MultiBlockVars_m
   USE Parameters_m, ONLY: wp
   USE GridTransformVars_m, ONLY: GridMetrics
   USE FlowVariables_m, ONLY: FlowVars
   USE ThermalGasVars_m, ONLY: SpeciesData, MixtureData

   IMPLICIT NONE

   !> Parallel computation purpose
   INTEGER :: nbp, ncpu

   !> Total number of domains and  blocks
   INTEGER :: ndomain
   INTEGER :: nblk
   !> number of ghost cells being used in computation
   INTEGER :: ngc
   !> number of ghost cells being stored in restart files
   INTEGER :: ngls

   TYPE MultiDomain
      INTEGER :: istart, iend, jstart, jend, kstart, kend
      INTEGER :: nblocks
      !> Grid coordinates
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: x, y, z
      !> used to indicate which type of boundary conditions will be used
      !> for surfaces surrounding the block.
      INTEGER :: bc_imin, bc_imax, bc_jmin, bc_jmax, bc_kmin, bc_kmax
      INTEGER, ALLOCATABLE, DIMENSION(:) :: blockID
   END TYPE

   !> Multiblocks related variables
   TYPE MultiBlock
      INTEGER :: isize, jsize, ksize
      INTEGER :: imin, imax, jmin, jmax, kmin, kmax
      INTEGER :: istart, iend, jstart, jend, kstart, kend
      INTEGER :: domainID
      !> CPU ID
      INTEGER :: rank = 0
      !> Grid coordinates
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: x, y, z
      !> Grid coordinates based on cell-center (Not currently used)
      !REAL(KIND=WP), ALLOCATABLE, DIMENSION(:,:,:) :: xc, yc, zc

      !> used to indicate which type of boundary conditions will be used
      !> for surfaces surrounding the block.
      INTEGER :: bc_imin, bc_imax, bc_jmin, bc_jmax, bc_kmin, bc_kmax

      !> neighbor array stores the index of neighbors surrounding the block
      INTEGER, DIMENSION(-1:1,-1:1,-1:1) :: neighbor

      !> GridMetrics related variables
      TYPE(GridMetrics) :: GM

      !> Flow variables
      TYPE(FlowVars) :: flow

      !> Species data
      TYPE(SpeciesData) :: spc

      !> Mixture data: Mixture average thermodynamic variables
      TYPE(MixtureData) :: Mix

   END TYPE MultiBlock


CONTAINS

!-----------------------------------------------------------------------------!
   SUBROUTINE AllocateMultiBlockXYZ(blk, imin, imax, &
                                         jmin, jmax, &
                                         kmin, kmax)
!-----------------------------------------------------------------------------!

      IMPLICIT NONE
      TYPE(MultiBlock), INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: imin, imax, jmin, jmax, kmin, kmax

      ALLOCATE(blk%x(imin:imax,jmin:jmax,kmin:kmax))
      ALLOCATE(blk%y(imin:imax,jmin:jmax,kmin:kmax))
      ALLOCATE(blk%z(imin:imax,jmin:jmax,kmin:kmax))

      !> Initialize with zero value
      blk%x = 0.0_wp
      blk%y = 0.0_wp
      blk%z = 0.0_wp

   END SUBROUTINE

END MODULE MultiBlockVars_m
