!> \file: PerfectGas.F90
!> \author: Sayop Kim
!> \brief: 

MODULE PerfectGas_m
   USE Parameters_m, ONLY: wp
   USE SpeciesVars_m   

   IMPLICIT NONE

CONTAINS

   FUNCTION EvaluatePressure(rho, T, Rmix) RESULT(P)
      IMPLICIT NONE
      REAL(KIND=wp), INTENT(IN) :: rho, Rmix, T
      REAL(KIND=wp) :: P

      P = rho * Rmix * T
   END FUNCTION

   FUNCTION EvaluateDensity(P, T, Rmix) RESULT(rho)
      IMPLICIT NONE
      REAL(KIND=wp), INTENT(IN) :: P, Rmix, T
      REAL(KIND=wp) :: rho

      rho = P / (Rmix * T)
   END FUNCTION

END MODULE
