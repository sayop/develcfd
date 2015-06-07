!> \file: MultiBlockVars.F90
!> \author: Sayop Kim
!> \brief: Provides variables related multiblocks grid data

MODULE MultiBlockVars_m
   USE Parameters_m, ONLY: wp

   IMPLICIT NONE

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
      !> Grid coordinates
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: x, y, z
      !> Grid coordinates based on cell-center (Not currently used)
      !REAL(KIND=WP), ALLOCATABLE, DIMENSION(:,:,:) :: xc, yc, zc

      !> used to indicate which type of boundary conditions will be used
      !> for surfaces surrounding the block.
      INTEGER :: bc_imin, bc_imax, bc_jmin, bc_jmax, bc_kmin, bc_kmax

      !> neighbor array stores the index of neighbors surrounding the block
      INTEGER, DIMENSION(-1:1,-1:1,-1:1) :: neighbor
   END TYPE MultiBlock


!   TYPE GridMetrics_t
   
!   END TYPE GridMetrics_t

END MODULE MultiBlockVars_m
