!> \file: SetMixtureData.F90
!> \author: Sayop Kim
!> \brief: 

MODULE SetMixtureData_m
   USE Parameters_m, ONLY: wp, Ru

   IMPLICIT NONE

CONTAINS

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
