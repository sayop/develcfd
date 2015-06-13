!> \file: SpeciesVars.F90
!> \author: Sayop Kim
!> \brief: Provides flow/thermodynamics variables in primitive and conservative forms.

MODULE SpeciesVars_m
   USE Parameters_m, ONLY: wp, Ru

   IMPLICIT NONE
   !> Number of species
   INTEGER :: nspec
   
   TYPE SpeciesGlobal
      CHARACTER(LEN=12) :: spcName
      REAL(KIND=wp) :: Lewis, MW
      REAL(KIND=wp) :: Cp, Cv, R
   END TYPE

   TYPE(SpeciesGlobal), ALLOCATABLE, DIMENSION(:) :: SPC

   TYPE SpeciesData
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:,:) :: MassFrac
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:,:) :: MoleFrac
      !> Specfic heat @constant pressure and @constant volume [J/(kG.K)]
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:,:) :: Cp, Cv
      REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: CpMix, CvMix
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

      IF(input_data%Species%gaslaw .NE. 'CPG') THEN
         ALLOCATE(spc%Cp(imin:imax, jmin:jmax, kmin:kmax, 1:nspec))
         ALLOCATE(spc%Cv(imin:imax, jmin:jmax, kmin:kmax, 1:nspec))
      END IF
   
   END SUBROUTINE   

END MODULE
