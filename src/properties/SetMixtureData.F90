!> \file: SetMixtureData.F90
!> \author: Sayop Kim
!> \brief: 

MODULE SetMixtureData_m
   USE Parameters_m, ONLY: wp, Ru

   IMPLICIT NONE

CONTAINS


!-----------------------------------------------------------------------------!
   SUBROUTINE UpdateTempFromEnthalpy(blk)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock
      USE PerfectGas_m, ONLY: EvaluateTempFromHmixCPG, EvaluateTempFromHmixTPG
      USE xml_data_input
      
      IMPLICIT NONE
      TYPE(MultiBlock), INTENT(INOUT) :: blk
      REAL(KIND=wp) :: Eint, Hmix
      INTEGER :: i, j, k

      PROCEDURE(EvaluateTempFromHmixCPG), POINTER :: EvaluateTempFromHmix

      SELECT CASE(input_data%Species%gaslaw)
      CASE('CPG')
         EvaluateTempFromHmix => EvaluateTempFromHmixCPG
      CASE('TPG')
         EvaluateTempFromHmix => EvaluateTempFromHmixTPG
      END SELECT

      !> Find temperature from mixture enthalpy based on perfect gas assumption
      DO k = blk%kstart, blk%kend
         DO j = blk%jstart, blk%jend
            DO i = blk%istart, blk%iend
               blk%flow%T(i,j,k) = EvaluateTempFromHmix(blk%flow%T(i,j,k), &
                                                        blk%mix%Hmix(i,j,k), &
                                                        blk%spc%MassFrac(i,j,k,:))
            END DO
         END DO
      END DO
      
   END SUBROUTINE


!-----------------------------------------------------------------------------!
   SUBROUTINE UpdateMixtureThermoVariables(blk, SPC, nspec)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock
      USE ThermalGasVars_m, ONLY: SpeciesGlobal
      USE PerfectGas_m, ONLY: EvaluateEnthalpyCPG, EvaluateEnthalpyTPG, &
                              EvaluatePressure, EvaluateInternalEnergy
      USE xml_data_input

      IMPLICIT NONE
      TYPE(MultiBlock), INTENT(INOUT) :: blk
      TYPE(SpeciesGlobal), DIMENSION(:), INTENT(IN) :: SPC
      INTEGER, INTENT(IN) :: nspec
      INTEGER :: ispc, i, j, k
      REAL(KIND=wp) :: Hmix, Rmix

      PROCEDURE(EvaluateEnthalpyCPG), POINTER :: EvaluateEnthalpy

      !> Evaluate pressure, MWmix, Rmix quantity: Note that Rmix is not local arrays
      !>                                          defined at every node points
      !> Evaluate enthalpy and internal energy of mixture for every points
      !> Internal energy is evaluated by subtracting PV work from enthalpy quantity.
      SELECT CASE(input_data%Species%gaslaw)
      CASE('CPG')
         EvaluateEnthalpy => EvaluateEnthalpyCPG
      CASE('TPG')
         EvaluateEnthalpy => EvaluateEnthalpyTPG
      END SELECT
 
      DO k = blk%kstart, blk%kend
         DO j = blk%jstart, blk%jend
            DO i = blk%istart, blk%iend
               !> Mixture molecular weight
               blk%mix%MWmix(i,j,k) = EvaluateMWmix(blk%spc%MassFrac(i,j,k,:))
               Rmix = Ru / blk%mix%MWmix(i,j,k)

               !> Pressure: Equation of State
               blk%flow%P(i,j,k) = EvaluatePressure(blk%flow%RHO(i,j,k), &
                                                    blk%flow%T(i,j,k), &
                                                    Rmix)

               !> Enthalpy of mixture
               Hmix = 0.0_wp
               DO ispc = 1, nspec
                  Hmix = Hmix + blk%spc%MassFrac(i,j,k,ispc) * &
                                EvaluateEnthalpy(ispc, blk%flow%T(i,j,k))
               END DO
               blk%mix%Hmix(i,j,k) = Hmix

               !> Internal energy of mixture
               blk%mix%Eint(i,j,k) = EvaluateInternalEnergy(blk%mix%Hmix(i,j,k), &
                                                            blk%flow%RHO(i,j,k), &
                                                            blk%flow%P(i,j,k))

               !> Total energy: Internal energy + Kinetic energy
               blk%flow%Etot(i,j,k) = blk%flow%RHO(i,j,k) * &
                                      ( blk%mix%Eint(i,j,k) + &
                                        0.5_wp * (blk%flow%U(i,j,k) ** 2 + &
                                                  blk%flow%V(i,j,k) ** 2 + &
                                                  blk%flow%W(i,j,k) ** 2) )
            END DO
         END DO
      END DO


   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE UpdateMoleFracData(spc, nspec)
!-----------------------------------------------------------------------------!
      USE ThermalGasVars_m, ONLY: SpeciesData
   
      IMPLICIT NONE
      TYPE(SpeciesData), INTENT(INOUT) :: spc
      INTEGER, INTENT(IN) :: nspec
      INTEGER :: ispc

      DO ispc = 1, nspec
!         spc%MoleFrac(:,;,:,ispc) = 
      END DO

   END SUBROUTINE

!-----------------------------------------------------------------------------!
!   SUBROUTINE UpdateMixtureMolecularWeight(species, mixture)
!-----------------------------------------------------------------------------!
!      USE ThermalGasVars_m, ONLY: SpeciesData, MixtureData, SPC, nspec
!
!      IMPLICIT NONE
!      
!      TYPE(SpeciesData), INTENT(IN) :: species
!      TYPE(MixtureData), INTENT(INOUT) :: mixture
!      INTEGER :: ispc
!
!      mixture%MWmix(:,:,:) = 0.0_wp
!      DO ispc = 1, nspec
!         mixture%MWmix(:,:,:) = mixture%MWmix(:,:,:) + & 
!                                species%MassFrac(:,:,:,ispc) / SPC(ispc)%MW
!      END DO
!      mixture%MWmix = 1.0_wp / mixture%MWmix
!
!   END SUBROUTINE


   FUNCTION EvaluateMWmix(mfr) RESULT(MWmix)
      USE ThermalGasVars_m, ONLY: SPC, nspec

      IMPLICIT NONE
      REAL(KIND=wp), DIMENSION(1:nspec), INTENT(IN) :: mfr
      REAL(KIND=wp) :: MWmix
      INTEGER :: ispc

      MWmix = 0.0_wp
      DO ispc = 1, nspec
         MWmix = MWmix + mfr(ispc) / SPC(ispc)%MW
      END DO
      MWmix = 1.0_wp / MWmix
   END FUNCTION

END MODULE
