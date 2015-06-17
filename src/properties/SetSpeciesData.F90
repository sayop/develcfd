!> \file: SetSpeciesData.F90
!> \author: Sayop Kim
!> \brief: 

MODULE SetSpeciesData_m
   USE Parameters_m, ONLY: wp, Ru

CONTAINS

!-----------------------------------------------------------------------------!
   SUBROUTINE SetSpeciesThermoData(SPC, nspc)
!-----------------------------------------------------------------------------!
      USE ThermalGasVars_m, ONLY: SpeciesGlobal
      USE FlowVariables_m, ONLY: Tref
      USE PerfectGas_m, ONLY: EvaluateCp, EvaluateEnthalpyTPG, EvaluateDensity

      IMPLICIT NONE
      TYPE(SpeciesGlobal), DIMENSION(:), INTENT(INOUT) :: SPC
      INTEGER, INTENT(IN) :: nspc
      INTEGER :: ispc

      WRITE(*,*) ""
      WRITE(*,*) "# Reading Chemkin thermodynamic coefficients and molecular &
                    weight..."
      DO ispc = 1, nspc
         CALL ReadChemkinThermoCoef(SPC, ispc)
      END DO

      DO ispc = 1, nspc
         CALL ReadMolecularWeight(SPC, ispc)
      END DO

      DO ispc = 1, nspc
         SPC(ispc)%Cp = EvaluateCp(ispc, Tref)
         SPC(ispc)%Href = EvaluateEnthalpyTPG(ispc, Tref)
         SPC(ispc)%R = Ru / SPC(ispc)%MW
         SPC(ispc)%Cv = SPC(ispc)%Cp - SPC(ispc)%R
      END DO

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE ReadMolecularWeight(SPC, ispc)
!-----------------------------------------------------------------------------!
      USE ThermalGasVars_m, ONLY: SpeciesGlobal

      IMPLICIT NONE
      TYPE(SpeciesGlobal), DIMENSION(:), INTENT(INOUT) :: SPC
      INTEGER, INTENT(IN) :: ispc
      INTEGER :: IOunit, IOerr, i
      CHARACTER(LEN=128) :: MWFILE
      CHARACTER(LEN=15) :: chdump, ckspc
      REAL(KIND=wp) :: rmw
      IOunit = 10
      MWFILE = 'data/mw.dat'

      IOerr = 0
      i = 0

      OPEN(UNIT=IOunit, FILE=MWFILE, IOSTAT=IOerr)

      DO 
         i = i + 1
         IF ((IOerr .NE. 0) .OR. (ckspc .EQ. 'END')) THEN
            write(*,*) ispc, IOerr, ckspc
            WRITE(*,*) "--------------------------------------------------------"
            WRITE(*,*) "WARNING: Error while reading molecular weight in mw.dat!"
            WRITE(*,*) "--------------------------------------------------------"
            EXIT
         END IF
         IF (i .EQ. 1) THEN
            READ(IOunit, *, IOSTAT=IOerr) chdump
         END IF
         READ(IOunit, '(A15,F15.10)', IOSTAT=IOerr) ckspc, rmw
         IF (SPC(ispc)%spcName .EQ. TRIM(ADJUSTL(ckspc))) THEN
            SPC(ispc)%MW = rmw
            EXIT
         END IF
      END DO

      CLOSE(IOunit)
   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE ReadChemKinThermoCoef(SPC, ispc)
!-----------------------------------------------------------------------------!
      USE ThermalGasVars_m, ONLY: SpeciesGlobal
      
      IMPLICIT NONE
      TYPE(SpeciesGlobal), DIMENSION(:), INTENT(INOUT) :: SPC
      INTEGER, INTENT(IN) :: ispc
      INTEGER :: IOunit, IOerr, i, j
      CHARACTER(LEN=128) :: THERMOFILE
      CHARACTER(LEN=15) :: chdump, ckspc
      IOunit = 10
      THERMOFILE = 'data/therm.dat'

      IOerr = 0
      i = 0
      j = 0

      OPEN(UNIT=IOunit, FILE=THERMOFILE, IOSTAT=IOerr)

      DO
         i = i + 1
         IF ((IOerr .NE. 0) .OR. (ckspc .EQ. 'END')) THEN
            WRITE(*,*) "--------------------------------------------------------------------"
            WRITE(*,*) "WARNING: Error while reading thermodynamic coefficient in therm.dat!"
            WRITE(*,*) "--------------------------------------------------------------------"
            EXIT
         END IF
         IF (i .EQ. 1) THEN
            READ(IOunit, *, IOSTAT=IOerr) chdump
            READ(IOunit, *, IOSTAT=IOerr) chdump
         END IF
         READ(IOunit, '(A15)', IOSTAT=IOerr) ckspc
         IF (SPC(ispc)%spcName .EQ. TRIM(ADJUSTL(ckspc))) THEN
            READ(IOunit, '(5(1pe15.8))',IOSTAT=IOerr) (SPC(ispc)%THCOEF(j), j = 1, 5)
            READ(IOunit, '(5(1pe15.8))',IOSTAT=IOerr) (SPC(ispc)%THCOEF(j), j = 6,10)
            READ(IOunit, '(4(1pe15.8))',IOSTAT=IOerr) (SPC(ispc)%THCOEF(j), j =11,14)
            EXIT
         ELSE
            READ(IOunit, *, IOSTAT=IOerr) chdump
            READ(IOunit, *, IOSTAT=IOerr) chdump
            READ(IOunit, *, IOSTAT=IOerr) chdump
         END IF
      END DO

      CLOSE(IOunit)
   END SUBROUTINE
END MODULE
