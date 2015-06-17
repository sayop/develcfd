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

   FUNCTION EvaluateInternalEnergy(H, RHO, P) RESULT(Eint)
      IMPLICIT NONE
      REAL(KIND=wp), INTENT(IN) :: H, RHO, P
      REAL(KIND=wp) :: Eint

      Eint = H - P / RHO
   END FUNCTION

   FUNCTION EvaluateEnthalpyFromEint(Eint, RHO, P) RESULT(H)
      IMPLICIT NONE
      REAL(KIND=wp), INTENT(IN) :: Eint, RHO, P
      REAL(KIND=wp) :: H

      H = Eint + P / RHO
   END FUNCTION

   FUNCTION EvaluateCp(ispc, T) RESULT(Cp)
      !> Update constant pressure specific heat for single species
      USE ThermalGasVars_m, ONLY: SPC
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: ispc
      REAL(KIND=wp), INTENT(IN) :: T
      REAL(KIND=wp) :: Cp
      INTEGER :: i

      !> Median temperature is 1000 K.
      IF (T .LE. 1000.0_wp) THEN
         i = 8 !! For low temperature fit
      ELSE
         i = 1 !! For high temperature fit
      END IF

      !> This below actually is Cp/Ru in dimensionless
      Cp = SPC(ispc)%THCOEF(i) + SPC(ispc)%THCOEF(i+1) * T + & 
                                 SPC(ispc)%THCOEF(i+2) * T ** 2 + &
                                 SPC(ispc)%THCOEF(i+3) * T ** 3 + &
                                 SPC(ispc)%THCOEF(i+4) * T ** 4

      !> Cp in [J/kmol.K]
      Cp = Cp * Ru

      !> Cp in [J/kg.K]
      Cp = Cp / SPC(ispc)%MW
   END FUNCTION

   FUNCTION EvaluateEnthalpyTPG(ispc, T) RESULT(Htpg)
      !> Update heat of formation for single species
      !> Can be used for updating TPG enthalpy value.
      USE ThermalGasVars_m, ONLY: SPC
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: ispc
      REAL(KIND=wp), INTENT(IN) :: T
      REAL(KIND=wp) :: Htpg
      INTEGER :: i

      !> Median temperature is 1000 K.
      IF (T .LE. 1000.0_wp) THEN
         i = 8 !! For low temperature fit
      ELSE
         i = 1 !! For high temperature fit
      END IF

      !> This below actually is Ho/(Ru.T) in dimensionless
      Htpg = SPC(ispc)%THCOEF(i) + SPC(ispc)%THCOEF(i+1) / 2.0_wp * T + &
                                 SPC(ispc)%THCOEF(i+2) / 3.0_wp * T ** 2 + &
                                 SPC(ispc)%THCOEF(i+3) / 4.0_wp * T ** 3 + &
                                 SPC(ispc)%THCOEF(i+4) / 5.0_wp * T ** 4 + &
                                 SPC(ispc)%THCOEF(i+5) / T

      !> Ho in [J/kmol]
      Htpg = Htpg * Ru * T

      !> Ho in [J/kg]
      Htpg = Htpg / SPC(ispc)%MW
   END FUNCTION

   FUNCTION EvaluateEnthalpyCPG(ispc, T) RESULT(Hcpg)
      !> Tref should be first evaluated in advance!!!
      !> This value is initialized in subroutine SetSpeciesThermoData.
      !> Plz make sure that this subroutine is called earlier than this function.
      USE FlowVariables_m, ONLY: Tref
      !> SPC element values should be first evaluated!!!!
      USE ThermalGasVars_m, ONLY: SPC
      !> Update absolute enthalpy for single species with CPG assumption
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: ispc
      REAL(KIND=wp), INTENT(IN) :: T
      REAL(KIND=wp) :: Hcpg

      Hcpg = SPC(ispc)%Href + (T - Tref) * SPC(ispc)%Cp
   END FUNCTION

   FUNCTION EvaluateTempFromHmixCPG(Hmix, mfr) RESULT(T)
      !> Find temperature from enthalpy of mixture
      USE FlowVariables_m, ONLY: Tref
      USE ThermalGasVars_m, ONLY: SPC, nspec
      IMPLICIT NONE
      REAL(KIND=wp), INTENT(IN) :: Hmix
      REAL(KIND=wp), DIMENSION(1:nspec), INTENT(IN) :: mfr
      REAL(KIND=wp) :: T, r1, r2
      INTEGER :: ispc

      r1 = 0.0_wp
      r2 = 0.0_wp
      DO ispc = 1, nspec
         r1 = r1 + mfr(ispc) * ( SPC(ispc)%Href - SPC(ispc)%Cp * Tref )
         r2 = r2 + mfr(ispc) * SPC(ispc)%Cp
      END DO

      T = ( Hmix - r1 ) / r2
   END FUNCTION

   FUNCTION EvaluateTempFromHmixTPG(Hmix, mfr) RESULT(T)
      !> Find temperature from enthalpy of mixture
      USE FlowVariables_m, ONLY: Tref
      USE ThermalGasVars_m, ONLY: SPC, nspec
      IMPLICIT NONE
      REAL(KIND=wp), INTENT(IN) :: Hmix
      REAL(KIND=wp), DIMENSION(1:nspec), INTENT(IN) :: mfr
      REAL(KIND=wp) :: T, r1, r2
      INTEGER :: ispc

      r1 = 0.0_wp
      r2 = 0.0_wp
      DO ispc = 1, nspec
         r1 = r1 + mfr(ispc) * ( SPC(ispc)%Href - SPC(ispc)%Cp * Tref )
         r2 = r2 + mfr(ispc) * SPC(ispc)%Cp
      END DO

      T = ( Hmix - r1 ) / r2
   END FUNCTION

END MODULE
