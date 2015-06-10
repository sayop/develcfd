!> \file: AllocateVariables.F90
!> \author: Sayop Kim

MODULE AllocateVars_m
   USE Parameters_m, ONLY: wp

CONTAINS

!-----------------------------------------------------------------------------!
   SUBROUTINE AllocateMultiBlockXYZ(blk, iblk, imin, imax, &
                                               jmin, jmax, &
                                               kmin, kmax)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: iblk
      INTEGER, INTENT(IN) :: imin, imax, jmin, jmax, kmin, kmax

      ALLOCATE(blk(iblk)%x(imin:imax,jmin:jmax,kmin:kmax))
      ALLOCATE(blk(iblk)%y(imin:imax,jmin:jmax,kmin:kmax))
      ALLOCATE(blk(iblk)%z(imin:imax,jmin:jmax,kmin:kmax))

      !> Initialize with zero value
      blk(iblk)%x = 0.0_wp
      blk(iblk)%y = 0.0_wp
      blk(iblk)%z = 0.0_wp

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE AllocateConservedVars(flowVar, iblk, nvar, imin, imax, &
                                                         jmin, jmax, &
                                                         kmin, kmax)
!-----------------------------------------------------------------------------!
      USE FlowVariables_m, ONLY: FlowVars

      IMPLICIT NONE
      TYPE(FlowVars), DIMENSION(:), INTENT(INOUT) :: flowVar
      INTEGER, INTENT(IN) :: iblk, nvar
      INTEGER, INTENT(IN) :: imin, imax, jmin, jmax, kmin, kmax

      ALLOCATE(flowVar(iblk)%Q(imin:imax, jmin:jmax , kmin:kmax, 1:nvar))

      !> Initialize with zero value
      flowVar(iblk)%Q = 0.0_wp

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE AllocatePrimitiveVars(flowVar, iblk, imin, imax, &
                                                   jmin, jmax, &
                                                   kmin, kmax)
!-----------------------------------------------------------------------------!
      USE FlowVariables_m, ONLY: FlowVars

      IMPLICIT NONE
      TYPE(FlowVars), DIMENSION(:), INTENT(INOUT) :: flowVar
      INTEGER, INTENT(IN) :: iblk
      INTEGER, INTENT(IN) :: imin, imax, jmin, jmax, kmin, kmax

      ALLOCATE(flowVar(iblk)%U(imin:imax, jmin:jmax , kmin:kmax))
      ALLOCATE(flowVar(iblk)%V(imin:imax, jmin:jmax , kmin:kmax))
      ALLOCATE(flowVar(iblk)%W(imin:imax, jmin:jmax , kmin:kmax))
      ALLOCATE(flowVar(iblk)%P(imin:imax, jmin:jmax , kmin:kmax))
      ALLOCATE(flowVar(iblk)%T(imin:imax, jmin:jmax , kmin:kmax))
      ALLOCATE(flowVar(iblk)%RHO(imin:imax, jmin:jmax , kmin:kmax))

      !> Initialize with zero value
      flowVar(iblk)%U = 0.0_wp
      flowVar(iblk)%V = 0.0_wp
      flowVar(iblk)%W = 0.0_wp
      flowVar(iblk)%P = 0.0_wp
      flowVar(iblk)%T = 0.0_wp
      flowVar(iblk)%RHO = 0.0_wp

   END SUBROUTINE

END MODULE
