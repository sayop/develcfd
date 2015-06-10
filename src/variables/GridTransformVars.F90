!> \file: GridTransformVars.F90
!> \author: Sayop Kim
!> \brief: Provides flow/thermodynamics variables in primitive and conservative forms.

MODULE GridTransformVars_m
   USE Parameters_m, ONLY: wp

   IMPLICIT NONE

   TYPE GridMetrics
      !> Grid Metrics
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: PXPI, PXPJ, PXPK
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: PYPI, PYPJ, PYPK
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: PZPI, PZPJ, PZPK
      !> Inverse grid metrics
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: PIPX, PIPY, PIPZ
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: PJPX, PJPY, PJPZ
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: PKPX, PKPY, PKPZ
      !> Grid Jacobian
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: GridJacobian
   END TYPE

CONTAINS

!-----------------------------------------------------------------------------!
   SUBROUTINE AllocateGridMetricsVars(GM, imin, imax, &
                                          jmin, jmax, &
                                          kmin, kmax)
!-----------------------------------------------------------------------------!

      IMPLICIT NONE
      TYPE(GridMetrics), INTENT(OUT) :: GM
      INTEGER, INTENT(IN) :: imin, imax, jmin, jmax, kmin, kmax

      ALLOCATE(GM%PXPI(imin:imax,jmin:jmax,kmin:kmax))
      ALLOCATE(GM%PXPJ(imin:imax,jmin:jmax,kmin:kmax))
      ALLOCATE(GM%PXPK(imin:imax,jmin:jmax,kmin:kmax))
      ALLOCATE(GM%PYPI(imin:imax,jmin:jmax,kmin:kmax))
      ALLOCATE(GM%PYPJ(imin:imax,jmin:jmax,kmin:kmax))
      ALLOCATE(GM%PYPK(imin:imax,jmin:jmax,kmin:kmax))
      ALLOCATE(GM%PZPI(imin:imax,jmin:jmax,kmin:kmax))
      ALLOCATE(GM%PZPJ(imin:imax,jmin:jmax,kmin:kmax))
      ALLOCATE(GM%PZPK(imin:imax,jmin:jmax,kmin:kmax))

   END SUBROUTINE

END MODULE
