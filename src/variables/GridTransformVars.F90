!> \file: GridTransformVars.F90
!> \author: Sayop Kim
!> \brief: Provides flow/thermodynamics variables in primitive and conservative forms.

MODULE GridTransformVars_m
   USE Parameters_m, ONLY: wp

   IMPLICIT NONE

   TYPE GridMetrics
      !> Inverse Grid Metrics
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: PXPI, PXPJ, PXPK
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: PYPI, PYPJ, PYPK
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: PZPI, PZPJ, PZPK
      !> Grid metrics
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: PIPX, PIPY, PIPZ
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: PJPX, PJPY, PJPZ
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: PKPX, PKPY, PKPZ
      !> Grid Jacobian
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: Jacobian
   END TYPE

CONTAINS

!-----------------------------------------------------------------------------!
   SUBROUTINE AllocateInverseGridMetricsArrays(GM, imin, imax, &
                                                   jmin, jmax, &
                                                   kmin, kmax)
!-----------------------------------------------------------------------------!

      IMPLICIT NONE
      TYPE(GridMetrics), INTENT(INOUT) :: GM
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

!-----------------------------------------------------------------------------!
   SUBROUTINE DeallocateInverseGridMetricsArrays(GM)
!-----------------------------------------------------------------------------!

      IMPLICIT NONE
      TYPE(GridMetrics), INTENT(INOUT) :: GM

      DEALLOCATE(GM%PXPI)
      DEALLOCATE(GM%PXPJ)
      DEALLOCATE(GM%PXPK)
      DEALLOCATE(GM%PYPI)
      DEALLOCATE(GM%PYPJ)
      DEALLOCATE(GM%PYPK)
      DEALLOCATE(GM%PZPI)
      DEALLOCATE(GM%PZPJ)
      DEALLOCATE(GM%PZPK)

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE AllocateGridJacobianArray(GM, imin, imax, &
                                            jmin, jmax, &
                                            kmin, kmax)
!-----------------------------------------------------------------------------!

      IMPLICIT NONE
      TYPE(GridMetrics), INTENT(INOUT) :: GM
      INTEGER, INTENT(IN) :: imin, imax, jmin, jmax, kmin, kmax

      ALLOCATE(GM%Jacobian(imin:imax,jmin:jmax,kmin:kmax))
   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE AllocateGridMetricsArrays(GM, imin, imax, &
                                            jmin, jmax, &
                                            kmin, kmax)
!-----------------------------------------------------------------------------!

      IMPLICIT NONE
      TYPE(GridMetrics), INTENT(INOUT) :: GM
      INTEGER, INTENT(IN) :: imin, imax, jmin, jmax, kmin, kmax

      ALLOCATE(GM%PIPX(imin:imax,jmin:jmax,kmin:kmax))
      ALLOCATE(GM%PIPY(imin:imax,jmin:jmax,kmin:kmax))
      ALLOCATE(GM%PIPZ(imin:imax,jmin:jmax,kmin:kmax))
      ALLOCATE(GM%PJPX(imin:imax,jmin:jmax,kmin:kmax))
      ALLOCATE(GM%PJPY(imin:imax,jmin:jmax,kmin:kmax))
      ALLOCATE(GM%PJPZ(imin:imax,jmin:jmax,kmin:kmax))
      ALLOCATE(GM%PKPX(imin:imax,jmin:jmax,kmin:kmax))
      ALLOCATE(GM%PKPY(imin:imax,jmin:jmax,kmin:kmax))
      ALLOCATE(GM%PKPZ(imin:imax,jmin:jmax,kmin:kmax))

   END SUBROUTINE

END MODULE
