!> \file: MultiDomain.F90
!> \author: Sayop Kim
!> \brief: Provides variables related multidomain grid data

MODULE MultiDomainVars_m
   USE Parameters_m, ONLY: wp

   IMPLICIT NONE

   !> Total number of domains
   INTEGER :: ndomains
   !> number of ghost cells being used in computation
   INTEGER :: ngc
   !> number of ghost cells being stored in restart files
   INTEGER :: ngls

   !> Multidomain related variables
   TYPE MultiDomain
      INTEGER :: isize, jsize, ksize
      INTEGER :: imin, imax, jmin, jmax, kmin, kmax
      INTEGER :: istart, iend, jstart, jend, kstart, kend
      !> Grid coordinates
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: x, y, z
      !> Grid coordinates based on cell-center (Not currently used)
      !REAL(KIND=WP), ALLOCATABLE, DIMENSION(:,:,:) :: xc, yc, zc

      !> used to indicate which type of boundary conditions will be used
      !> for surfaces surrounding the domain.
      INTEGER :: bc_imin, bc_imax, bc_jmin, bc_jmax, bc_kmin, bc_kmax

      !> neighbor array stores the index of neighbors surrounding the domain
      INTEGER, DIMENSION(-1:1,-1:1,-1:1) :: neighbor
   END TYPE MultiDomain


!   TYPE GridMetrics_t
   
!   END TYPE GridMetrics_t

END MODULE MultiDomainVars_m
