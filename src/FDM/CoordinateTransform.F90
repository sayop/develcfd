!> \file: CoordinateTransform.F90
!> \author: Sayop Kim

MODULE CoordinateTransform_m
   USE Parameters_m, ONLY: wp
   USE GlobalVars_m, ONLY: rank

CONTAINS

!-----------------------------------------------------------------------------!
   SUBROUTINE SetGridMetrics(blk, nblk, norder)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock
      USE GridTransformVars_m, ONLY: AllocateInverseGridMetricsArrays, &
                                     DeallocateInverseGridMetricsArrays, &
                                     AllocateGridJacobianArray, &
                                     AllocateGridMetricsArrays

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: nblk, norder
      INTEGER :: iblk


      IF (rank .EQ. 0) THEN
         WRITE(*,*) ""
         IF (norder .EQ. 2) THEN
            WRITE(*,*) "# Setting up Grid Metrics in 2nd order accuracy..."
         ELSE
            WRITE(*,*) "# Setting up Grid Metrics in 4th order accuracy..."
         END IF
      END IF

      IF ((norder .NE. 2) .AND. (norder .NE. 4)) THEN
         WRITE(*,*) "---------------------------------------------------------"
         WRITE(*,*) "WARNING: Only 2nd and 4th order of accuracy are available"
         WRITE(*,*) "for central-difference in grid metrics evaluation"
         WRITE(*,*) "---------------------------------------------------------"
         STOP
      END IF

      DO iblk = 1, nblk
         CALL AllocateInverseGridMetricsArrays(blk(iblk)%GM, &
                                           blk(iblk)%istart, blk(iblk)%iend, &
                                           blk(iblk)%jstart, blk(iblk)%jend, &
                                           blk(iblk)%kstart, blk(iblk)%kend)

         CALL AllocateGridJacobianArray(blk(iblk)%GM, &
                                        blk(iblk)%istart, blk(iblk)%iend, &
                                        blk(iblk)%jstart, blk(iblk)%jend, &
                                        blk(iblk)%kstart, blk(iblk)%kend)

         CALL AllocateGridMetricsArrays(blk(iblk)%GM, &
                                        blk(iblk)%istart, blk(iblk)%iend, &
                                        blk(iblk)%jstart, blk(iblk)%jend, &
                                        blk(iblk)%kstart, blk(iblk)%kend)

         !> Here Grid Jacobian will be evaluated simultaneously!
         CALL UpdateInverseGridMetrics(blk(iblk), norder)

         !> Update grid metrics arrays that are used for generalized 
         !> transformation of flow variables and computational grid.
         CALL UpdateGridMetrics(blk(iblk))

         !> Inverse Grid Metrics is temporarily used for evalution of 
         !> grid metrics and grid jacobian.
         CALL DeallocateInverseGridMetricsArrays(blk(iblk)%GM)
      END DO
   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE UpdateGridMetrics(blk)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock

      IMPLICIT NONE
      TYPE(MultiBlock), INTENT(INOUT) :: blk
      INTEGER :: i, j, k
      REAL(KIND=wp) :: Jacobian, PXPI, PYPI, PZPI, &
                                 PXPJ, PYPJ, PZPJ, &
                                 PXPK, PYPK, PZPK

      DO k = blk%kstart, blk%kend
         DO j = blk%jstart, blk%jend
            DO i = blk%istart, blk%iend
               Jacobian = blk%GM%Jacobian(i,j,k)
               PXPI     = blk%GM%PXPI(i,j,k)
               PYPI     = blk%GM%PYPI(i,j,k)
               PZPI     = blk%GM%PZPI(i,j,k)
               PXPJ     = blk%GM%PXPJ(i,j,k)
               PYPJ     = blk%GM%PYPJ(i,j,k)
               PZPJ     = blk%GM%PZPJ(i,j,k)
               PXPK     = blk%GM%PXPK(i,j,k)
               PYPK     = blk%GM%PYPK(i,j,k)
               PZPK     = blk%GM%PZPK(i,j,k)

               !> Evaluate inverse grid metrics
               blk%GM%PIPX(i,j,k) =  Jacobian * (PYPJ * PZPK - PYPK * PZPJ)
               blk%GM%PIPY(i,j,k) = -Jacobian * (PXPJ * PZPK - PXPK * PZPJ)
               blk%GM%PIPZ(i,j,k) =  Jacobian * (PXPJ * PYPK - PXPK * PYPJ)
               blk%GM%PJPX(i,j,k) = -Jacobian * (PYPI * PZPK - PYPK * PZPI)
               blk%GM%PJPY(i,j,k) =  Jacobian * (PXPI * PZPK - PXPK * PZPI)
               blk%GM%PJPZ(i,j,k) = -Jacobian * (PXPI * PYPK - PXPK * PYPI)
               blk%GM%PKPX(i,j,k) =  Jacobian * (PYPI * PZPJ - PYPJ * PZPI)
               blk%GM%PKPY(i,j,k) = -Jacobian * (PXPI * PZPJ - PXPJ * PZPI)
               blk%GM%PKPZ(i,j,k) =  Jacobian * (PXPI * PYPJ - PXPJ * PYPI)
            END DO
         END DO
      END DO

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE UpdateInverseGridMetrics(blk, norder)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock

      IMPLICIT NONE
      TYPE(MultiBlock), INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: norder
      INTEGER :: i, j, k
      PROCEDURE(CentralDiff2ndOrder), POINTER :: CentralDiff


      SELECT CASE(norder)
      CASE(2)
         CentralDiff => CentralDiff2ndOrder
      CASE(4)
         CentralDiff => CentralDiff4thOrder
      END SELECT


      DO k = blk%kstart, blk%kend
         DO j = blk%jstart, blk%jend
            DO i = blk%istart, blk%iend
               !> Evaluate inverse grid metrics
               blk%GM%PXPI(i,j,k) = CentralDiff(blk%x, blk%imin, blk%imax, &
                                                blk%jmin, blk%jmax, &
                                                blk%kmin, blk%kmax, &
                                                i, j, k, 1)
               blk%GM%PYPI(i,j,k) = CentralDiff(blk%y, blk%imin, blk%imax, &
                                                blk%jmin, blk%jmax, &
                                                blk%kmin, blk%kmax, &
                                                i, j, k, 1)
               blk%GM%PZPI(i,j,k) = CentralDiff(blk%z, blk%imin, blk%imax, &
                                                blk%jmin, blk%jmax, &
                                                blk%kmin, blk%kmax, &
                                                i, j, k, 1)
               blk%GM%PXPJ(i,j,k) = CentralDiff(blk%x, blk%imin, blk%imax, &
                                                blk%jmin, blk%jmax, &
                                                blk%kmin, blk%kmax, &
                                                i, j, k, 2)
               blk%GM%PYPJ(i,j,k) = CentralDiff(blk%y, blk%imin, blk%imax, &
                                                blk%jmin, blk%jmax, &
                                                blk%kmin, blk%kmax, &
                                                i, j, k, 2)
               blk%GM%PZPJ(i,j,k) = CentralDiff(blk%z, blk%imin, blk%imax, &
                                                blk%jmin, blk%jmax, &
                                                blk%kmin, blk%kmax, &
                                                i, j, k, 2)
               blk%GM%PXPK(i,j,k) = CentralDiff(blk%x, blk%imin, blk%imax, &
                                                blk%jmin, blk%jmax, &
                                                blk%kmin, blk%kmax, &
                                                i, j, k, 3)
               blk%GM%PYPK(i,j,k) = CentralDiff(blk%y, blk%imin, blk%imax, &
                                                blk%jmin, blk%jmax, &
                                                blk%kmin, blk%kmax, &
                                                i, j, k, 3)
               blk%GM%PZPK(i,j,k) = CentralDiff(blk%z, blk%imin, blk%imax, &
                                                blk%jmin, blk%jmax, &
                                                blk%kmin, blk%kmax, &
                                                i, j, k, 3)
               !> Evaluate grid Jacobian
               blk%GM%Jacobian(i,j,k) = Jacobian(blk%GM%PXPI(i,j,k), &
                                                 blk%GM%PYPI(i,j,k), & 
                                                 blk%GM%PYPI(i,j,k), & 
                                                 blk%GM%PXPJ(i,j,k), & 
                                                 blk%GM%PYPJ(i,j,k), & 
                                                 blk%GM%PZPJ(i,j,k), & 
                                                 blk%GM%PXPK(i,j,k), & 
                                                 blk%GM%PYPK(i,j,k), & 
                                                 blk%GM%PZPK(i,j,k))

               IF (1.0_wp / blk%GM%Jacobian(i,j,k) .LE. 1.0E-16_wp) THEN
                  WRITE(*,*) "-----------------------------------"
                  WRITE(*,*) "WARNING: Singularity cell exists!!!"
                  WRITE(*,*) "Please check grid quality."
                  WRITE(*,*) "-----------------------------------"
                  STOP
               END IF
            END DO
         END DO
      END DO

   END SUBROUTINE

   FUNCTION CentralDiff2ndOrder(xyz, imin, imax, jmin, jmax, kmin, kmax, &
                                i, j, k, ndir) RESULT(outcome)

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: imin, imax, jmin, jmax, kmin, kmax
      REAL(KIND=wp), DIMENSION(imin:imax,jmin:jmax,kmin:kmax), INTENT(IN) :: xyz
      INTEGER, INTENT(IN) :: i, j, k, ndir
      REAL(KIND=wp) :: outcome

      SELECT CASE(ndir)
      CASE(1)
         outcome = 0.5_wp * (xyz(i+1,j,k) - xyz(i-1,j,k))
      CASE(2)
         outcome = 0.5_wp * (xyz(i,j+1,k) - xyz(i,j-1,k))
      CASE(3)
         outcome = 0.5_wp * (xyz(i,j,k+1) - xyz(i,j,k-1))
      END SELECT

   END FUNCTION CentralDiff2ndOrder

   FUNCTION CentralDiff4thOrder(xyz, imin, imax, jmin, jmax, kmin, kmax, &
                                i, j, k, ndir) RESULT(outcome)

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: imin, imax, jmin, jmax, kmin, kmax
      REAL(KIND=wp), DIMENSION(imin:imax,jmin:jmax,kmin:kmax), INTENT(IN) :: xyz
      INTEGER, INTENT(IN) :: i, j, k, ndir
      REAL(KIND=wp) :: outcome

      SELECT CASE(ndir)
      CASE(1)
         outcome = (-xyz(i+2,j,k) + 8.0_wp*xyz(i+1,j,k) - &
                    8.0_wp*xyz(i-1,j,k) + xyz(i-2,j,k)) / 12.0_wp
      CASE(2)
         outcome = (-xyz(i,j+2,k) + 8.0_wp*xyz(i,j+1,k) - &
                    8.0_wp*xyz(i,j-1,k) + xyz(i,j-2,k)) / 12.0_wp
      CASE(3)
         outcome = (-xyz(i,j,k+2) + 8.0_wp*xyz(i,j,k+1) - &
                    8.0_wp*xyz(i,j,k-1) + xyz(i,j,k-2)) / 12.0_wp
      END SELECT

   END FUNCTION CentralDiff4thOrder
   
   FUNCTION Jacobian(PXPI, PYPI, PZPI, &
                     PXPJ, PYPJ, PZPJ, &
                     PXPK, PYPK, PZPK) RESULT(outcome)

      IMPLICIT NONE
      REAL(KIND=wp), INTENT(IN) :: PXPI, PYPI, PZPI, &
                                   PXPJ, PYPJ, PZPJ, &
                                   PXPK, PYPK, PZPK
      REAL(KIND=wp) :: outcome ! output is Grid Jacobian

      outcome = PXPI * (PYPJ * PZPK - PYPK * PZPJ) + &
                PXPJ * (PYPK * PZPI - PYPI * PZPK) + &
                PXPK * (PYPI * PZPJ - PYPJ * PZPI)
      outcome = 1.0_wp / outcome
   END FUNCTION

END MODULE

