!> \file: CoordinateTransform.F90
!> \author: Sayop Kim

MODULE CoordinateTransform_m
   USE Parameters_m, ONLY: wp

CONTAINS

!-----------------------------------------------------------------------------!
   SUBROUTINE SetGridMetrics(blk, nblk, norder)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock
      USE GridTransformVars_m, ONLY: AllocateGridMetricsVars

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), TARGET, INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: nblk, norder
      INTEGER :: iblk


      DO iblk = 1, nblk
         CALL AllocateGridMetricsVars(blk(iblk)%GM, &
                                      blk(iblk)%istart, blk(iblk)%iend, &
                                      blk(iblk)%jstart, blk(iblk)%jend, &
                                      blk(iblk)%kstart, blk(iblk)%kend)

         SELECT CASE(norder)
         CASE(2)

         CASE(3)

         END SELECT
      END DO
   END SUBROUTINE

END MODULE
