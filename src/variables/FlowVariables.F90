!> \file: FlowVariables.F90
!> \author: Sayop Kim
!> \brief: Provides flow/thermodynamics variables in primitive and conservative forms.

MODULE FlowVariables_m
   USE Parameters_m, ONLY: wp

   IMPLICIT NONE

   REAL(KIND=wp) :: Tref, Pref, Uref, Vref, Wref

   TYPE FlowVars
      !> Conservative variables
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:,:) :: Q
      !> Primitive variables
      !>> Velocity components
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: U, V, W
      !>> Pressure
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: P
      !> Density
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: RHO
      !> Temperature
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: T
      !> Mixture total energy per unit mass: Etot
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: Etot
   END TYPE

CONTAINS

!-----------------------------------------------------------------------------!
   SUBROUTINE AllocateConservedVars(flow, nvar, imin, imax, &
                                                jmin, jmax, &
                                                kmin, kmax)
!-----------------------------------------------------------------------------!

      IMPLICIT NONE
      TYPE(FlowVars), INTENT(INOUT) :: flow
      INTEGER, INTENT(IN) :: nvar
      INTEGER, INTENT(IN) :: imin, imax, jmin, jmax, kmin, kmax

      ALLOCATE(flow%Q(imin:imax, jmin:jmax , kmin:kmax, 1:nvar))

      !> Initialize with zero value
      flow%Q = 0.0_wp

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE AllocatePrimitiveVars(flow, imin, imax, &
                                          jmin, jmax, &
                                          kmin, kmax)
!-----------------------------------------------------------------------------!

      IMPLICIT NONE
      TYPE(FlowVars), INTENT(INOUT) :: flow
      INTEGER, INTENT(IN) :: imin, imax, jmin, jmax, kmin, kmax

      ALLOCATE(flow%U(imin:imax, jmin:jmax, kmin:kmax))
      ALLOCATE(flow%V(imin:imax, jmin:jmax, kmin:kmax))
      ALLOCATE(flow%W(imin:imax, jmin:jmax, kmin:kmax))
      ALLOCATE(flow%P(imin:imax, jmin:jmax, kmin:kmax))
      ALLOCATE(flow%T(imin:imax, jmin:jmax, kmin:kmax))
      ALLOCATE(flow%RHO(imin:imax, jmin:jmax, kmin:kmax))

      !> Initialize with zero value
      flow%U = 0.0_wp
      flow%V = 0.0_wp
      flow%W = 0.0_wp
      flow%P = 0.0_wp
      flow%T = 0.0_wp
      flow%RHO = 0.0_wp

   END SUBROUTINE

END MODULE FlowVariables_m
