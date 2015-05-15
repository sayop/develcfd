!> \file: MultiDomain.F90
!> \author: Sayop Kim
!> \brief: Provides variables related multidomain grid data

MODULE MultiDomainVars_m
   USE Parameters_m, ONLY: wp

   IMPLICIT NONE

   !> Multidomain related variables
   TYPE MultiDomain_t
      INTEGER :: imin, jmin, kmin
      INTEGER :: imax, jmax, kmax
      !> number of ghost cells being used in computation
      INTEGER :: ngc
      !> number of ghost cells being stored in restart files
      INTEGER :: ngls
      !> Grid coordinates
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: x, y, z
      !> Grid coordinates based on cell-center
      REAL(KIND=WP), ALLOCATABLE, DIMENSION(:,:,:) :: xc, yc, zc
   END TYPE MultiDomain_t

END MODULE
