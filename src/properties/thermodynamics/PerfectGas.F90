!> \file: PerfectGas.F90
!> \author: Sayop Kim
!> \brief: 

MODULE PerfectGas_m
   USE Parameters_m, ONLY: wp, Ru
   USE ThermalGasVars_m   

   IMPLICIT NONE

CONTAINS

   FUNCTION EvaluatePressure(rho, T, R) RESULT(P)
      IMPLICIT NONE
      REAL(KIND=wp), INTENT(IN) :: rho, R, T
      REAL(KIND=wp) :: P

      P = rho * R * T
   END FUNCTION

   FUNCTION EvaluateDensity(P, T, R) RESULT(rho)
      IMPLICIT NONE
      REAL(KIND=wp), INTENT(IN) :: P, R, T
      REAL(KIND=wp) :: rho

      rho = P / (R * T)
   END FUNCTION

   FUNCTION EvaluateInternalEnergy(Cv, T) RESULT(e)
      IMPLICIT NONE
      REAL(KIND=wp), INTENT(IN) :: Cv, T
      REAL(KIND=wp) :: e

      e = Cv * T
   END FUNCTION

   FUNCTION EvaluateCp(T, Tmed, MW, THCOEF) RESULT(Cp)
      !> Update constant pressure specific heat for single species
      IMPLICIT NONE
      REAL(KIND=wp), INTENT(IN) :: T, Tmed, MW
      REAL(KIND=wp), DIMENSION(14), INTENT(IN) :: THCOEF
      REAL(KIND=wp) :: Cp
      INTEGER :: i

      IF (T .LE. Tmed) THEN
         i = 8 !! For low temperature fit
      ELSE
         i = 1 !! For high temperature fit
      END IF

      !> This below actually is Cp/Ru in dimensionless
      Cp = THCOEF(i) + THCOEF(i+1) * T + THCOEF(i+2) * T ** 2 + &
           THCOEF(i+3) * T ** 3 + THCOEF(i+4) * T ** 4

      !> Cp in [J/kmol.K]
      Cp = Cp * Ru

      !> Cp in [J/kg.K]
      Cp = Cp / MW
   END FUNCTION

   FUNCTION EvaluateHf(T, Tmed, MW, THCOEF) RESULT(Hf)
      !> Update heat of formation for single species
      !> Can be used for updating TPG enthalpy value.
      IMPLICIT NONE
      REAL(KIND=wp), INTENT(IN) :: T, Tmed, MW
      REAL(KIND=wp), DIMENSION(14), INTENT(IN) :: THCOEF
      REAL(KIND=wp) :: Hf
      INTEGER :: i

      !> This below actually is Ho/(Ru.T) in dimensionless
      Hf = THCOEF(i) + THCOEF(i+1) / 2.0_wp * T + &
                       THCOEF(i+2) / 3.0_wp * T ** 2 + &
                       THCOEF(i+3) / 4.0_wp * T ** 3 + &
                       THCOEF(i+4) / 5.0_wp * T ** 4 + &
                       THCOEF(i+5) / T

      !> Ho in [J.kmol.K]
      Hf = Hf * Ru * T

      !> Ho in [J/kg.K]
      Hf = Hf / MW
   END FUNCTION

   FUNCTION EvaluateEnthalpyCPG(Hf, Cp, Tref, T) RESULT(Hcpg)
      !> Update absolute enthalpy for single species
      IMPLICIT NONE
      REAL(KIND=wp), INTENT(IN) :: T, Tref, Hf, Cp
      REAL(KIND=wp) :: Hcpg

      Hcpg = Hf + (T - Tref) * Cp
   END FUNCTION

END MODULE
