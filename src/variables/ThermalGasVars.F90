!> \file: ThermalGasVars.F90
!> \author: Sayop Kim
!> \brief: Provides flow/thermodynamics variables in primitive and conservative forms.

MODULE ThermalGasVars_m
   USE Parameters_m, ONLY: wp, Ru

   IMPLICIT NONE
   !> Number of species
   INTEGER :: nspec
   
   TYPE SpeciesGlobal
      CHARACTER(LEN=12) :: spcName
      REAL(KIND=wp) :: Lewis, MW
      REAL(KIND=wp) :: Cp, Cv, R
      !> Heat of formation at standard state & internal energy for single species
      REAL(KIND=wp) :: Href, EintRef

      !> Chemkin Thermo property coefficient
      REAL(KIND=wp), DIMENSION(14) :: THCOEF
   END TYPE

   TYPE(SpeciesGlobal), ALLOCATABLE, DIMENSION(:) :: SPC

   TYPE SpeciesData
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:,:) :: MassFrac
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:,:) :: MoleFrac
      !> Specfic heat @constant pressure and @constant volume [J/(kG.K)]
!      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:,:) :: Cp, Cv
   END TYPE

   TYPE MixtureData
      !> Internal energy of mixture per unit mass
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: Eint
      !> Absolute enthalpy of mixture per unit mass
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: Hmix
!      !> Mixture gas constant: Rmix: This is removed because this is readily
       !>                             evaluated from Ru / MWmix
!      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: Rmix
      !> Mixture gas specific heats: Cp, Cv
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: Cpmix
      !> Mixture molecular weight
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: MWmix
   END TYPE

CONTAINS

!-----------------------------------------------------------------------------!
   SUBROUTINE AllocateSpeciesData(spc, nspec, imin, imax, &
                                              jmin, jmax, &
                                              kmin, kmax)
!-----------------------------------------------------------------------------!
      USE xml_data_input

      IMPLICIT NONE
      TYPE(SpeciesData), INTENT(INOUT) :: spc
      INTEGER, INTENT(IN) :: nspec
      INTEGER, INTENT(IN) :: imin, imax, jmin, jmax, kmin, kmax

      ALLOCATE(spc%MassFrac(imin:imax, jmin:jmax, kmin:kmax, 1:nspec))
      ALLOCATE(spc%MoleFrac(imin:imax, jmin:jmax, kmin:kmax, 1:nspec))

!      IF(input_data%Species%gaslaw .NE. 'CPG') THEN
!         ALLOCATE(spc%Cp(imin:imax, jmin:jmax, kmin:kmax, 1:nspec))
!         ALLOCATE(spc%Cv(imin:imax, jmin:jmax, kmin:kmax, 1:nspec))
!      END IF
   
   END SUBROUTINE   

!-----------------------------------------------------------------------------!
   SUBROUTINE AllocateMixtureData(mix, imin, imax, &
                                       jmin, jmax, &
                                       kmin, kmax)
!-----------------------------------------------------------------------------!
      USE xml_data_input

      IMPLICIT NONE
      TYPE(MixtureData), INTENT(INOUT) :: mix
      INTEGER, INTENT(IN) :: imin, imax, jmin, jmax, kmin, kmax

      ALLOCATE(mix%Eint(imin:imax, jmin:jmax, kmin:kmax))
      ALLOCATE(mix%Hmix(imin:imax, jmin:jmax, kmin:kmax))
!      ALLOCATE(mix%Rmix(imin:imax, jmin:jmax, kmin:kmax))
      ALLOCATE(mix%MWmix(imin:imax, jmin:jmax, kmin:kmax))

      IF(input_data%Species%gaslaw .NE. 'CPG') THEN
         ALLOCATE(mix%Cpmix(imin:imax, jmin:jmax, kmin:kmax))
      END IF                                  
                                              
   END SUBROUTINE 

END MODULE
