!> \file: SetupSimulation.F90
!> \author: Sayop Kim

MODULE SetupSimulation_m
   USE Parameters_m, ONLY: wp

CONTAINS

!-----------------------------------------------------------------------------!
   SUBROUTINE InitializeSimulation()
!-----------------------------------------------------------------------------!
      USE GlobalVars_m, ONLY: MPI_COMM_WORLD, rank, ncpu, istop
      USE MultiBlockVars_m, ONLY: nblk, nbp
      USE IO_m

      IMPLICIT NONE
      INTEGER :: ierr, nprocs
      CHARACTER(LEN=128) :: message

      !> Initialize switch for simulation stop
      istop = 0

#ifndef SERIAL
      CALL MPI_INIT(ierr)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
#endif

   !> Read xml input file and initialize basic block related variables: ndomain, nblk, 
   !>                                                                   ngc, ngls, ncpu
   CALL ReadInputFiles()
   !> Write new_input.xml for back-up
   CALL WriteInputFiles()


#ifndef SERIAL
      IF ((ncpu .NE. nprocs) .AND. (rank .EQ. 0)) THEN
         WRITE(message,"(A,I4)") "WARNING: You are using incorrect CPU number: ", nprocs
         CALL PrintErrorMessage(message, istop)
!         WRITE(*,*)          "-------------------------------------------------"
!         WRITE(*,'(A46,I4)') "WARNING: You are using incorrect CPU number: ", nprocs
!         WRITE(*,*)          "-------------------------------------------------"
!         STOP
      END IF
      CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)
#endif

      !> Read load balancing info
!      CALL ReadLoadBalanceInfo(nblk, ncpu)
      nbp = nblk

   END SUBROUTINE

#ifndef SERIAL
!-----------------------------------------------------------------------------!
   SUBROUTINE ReadLoadBalanceInfo(nblk, ncpu)
!-----------------------------------------------------------------------------!
      USE GlobalVars_m, ONLY: MPI_COMM_WORLD, rank, &
                              blkInThisRank, RankToBlk, LocalBlkIndxInRank
      USE MultiBlockVars_m, ONLY: nbp

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: nblk, ncpu
      CHARACTER(LEN=128) :: LBFILE
      CHARACTER :: chdump
      INTEGER :: IOunit = 10, IOerr, ierr
      INTEGER :: ndump, n, indx
      INTEGER, ALLOCATABLE, DIMENSION(:) :: blkdump

      IF (rank .EQ. 0) THEN
         WRITE(*,*) ''
         WRITE(*,*) '# Reading BLK_DIST.DATA for Parallel Computation with MPI'
      END IF
      CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)

      LBFILE = 'BLK_DIST.DATA'
      OPEN(UNIT=IOunit, FILE=LBFILE, IOSTAT=IOerr)
      !> Read number of blocks
      READ(IOunit, '(A18,I5)', IOSTAT=IOerr) chdump, ndump
      !> Check whether nblock from read is same as the nblk of global variable.
      IF ((ndump .NE. nblk) .AND. (rank .EQ. 0)) THEN
         WRITE(*,*) "--------------------------------------------------&
                     ------------------------------"
         WRITE(*,*) "WARNING: Please check 'nblk' in the input file or &
                     re-create BLK_DIST.DATA file!!"
         WRITE(*,*) "--------------------------------------------------&
                     ------------------------------"
         STOP
      END IF
      !> Wait till other processors are done to make sure get this point.
      CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)

      !> Read number of CPUs
      READ(IOunit, '(A18,I5)', IOSTAT=IOerr) chdump, ndump
      !> Check whether number of CPUs is coorect
      IF (ndump .NE. ncpu) THEN
         IF (rank .EQ. 0) THEN
            WRITE(*,*) "------------------------------------------------------&
                        ---------------------------------"
            WRITE(*,*) "WARNING: Please check number of CPUs for your MPI run &
                        or re-create BLK_DIST.DATA file!!"
            WRITE(*,*) "------------------------------------------------------&
                        ---------------------------------"
         END IF
         STOP
      END IF
      !> Wait till other processors are done to make sure get this point.
      CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)

      !>
      !> Read block distribution for each rank
      !>
      !> Read the characters and dump them out!
      READ(IOunit, *) chdump
      !> This will temporarily store blkID before assigning it into rnk%blkID array
      ALLOCATE(blkdump(1:nblk))
      ALLOCATE(RankToBlk(1:nblk))
      ALLOCATE(LocalBlkIndxInRank(1:nblk))
      !> Allocate block ID and rank storage arrays
      DO n = 0, ncpu-1
         READ(IOunit, *) ndump, nbp, blkdump(1:nbp)
         !> Here, ndump is rank ID
         !> nbp: number of block in this rank
         !> blkdump will contain global block ID temporarily.
         DO indx = 1, nbp
            RankToBlk(blkdump(indx)) = ndump
            LocalBlkIndxInRank(blkdump(indx)) = indx
         END DO
         IF (n .EQ. rank) THEN
            ALLOCATE(blkInThisRank(nbp))
            blkInThisRank(1:nbp) = blkdump(1:nbp)
         END IF
      END DO
      
      !> Update 'nbp' value for this rank
      nbp = SIZE(blkInThisRank)

      !> Dump blkdump out!!
      DEALLOCATE(blkdump)      
      CLOSE(IOunit)

   END SUBROUTINE
#endif


#ifndef SERIAL
!-----------------------------------------------------------------------------!
   SUBROUTINE FinalizeParallelComputing()
!-----------------------------------------------------------------------------!
      USE GlobalVars_m, ONLY: MPI_COMM_WORLD, rank

      IMPLICIT NONE
      INTEGER :: ierr

      CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      CALL MPI_FINALIZE(ierr)
      IF (rank .EQ. 0) THEN
         WRITE(*,*) ""
         WRITE(*,*) "# Finalizing MPI communication!!"
      END IF
   END SUBROUTINE
#endif


!-----------------------------------------------------------------------------!
   SUBROUTINE InitializeMultiBlock(blk, nbp, ngc)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock
      USE InitMultiBlock_m

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: blk
      INTEGER, INTENT(IN) :: nbp, ngc
      INTEGER :: iblk

      ALLOCATE(blk(nbp))

      !> Read NODE files
      CALL ReadNODEfiles(blk, nbp, ngc)

      !> Read from grid file in plot3d file format
      !> Each block is simulataneously intialized and the variables are
      !> allocated while reading the grid data.
      !CALL ReadStructuredGrid(blk, nbp, ngc)

      !> Read bc info for every surfaces surrouding the block
      !> Will read from bcinfo.dat
      !CALL ReadBCinfo(nbp, blk)

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE InitializeFlowVars(blk, nblk)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock
      USE FlowVariables_m, ONLY: FlowVars, AllocateConservedVars, &
                                 AllocatePrimitiveVars
      USE CommunicateData_m

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: nblk
      INTEGER :: iblk, nvar

      !> Number of conserved variables
      nvar = 5

      DO iblk = 1, nblk
         !> Allocate Q array: Conserved variables
         CALL AllocateConservedVars(blk(iblk)%flow, nvar, blk(iblk)%imin, &
                                                          blk(iblk)%imax, &
                                                          blk(iblk)%jmin, &
                                                          blk(iblk)%jmax, &
                                                          blk(iblk)%kmin, &
                                                          blk(iblk)%kmax)
         !>Allocate primitive variables: u,v,w,P,T,density, and etc.
         CALL AllocatePrimitiveVars(blk(iblk)%flow, blk(iblk)%imin, &
                                                    blk(iblk)%imax, &
                                                    blk(iblk)%jmin, &
                                                    blk(iblk)%jmax, &
                                                    blk(iblk)%kmin, &
                                                    blk(iblk)%kmax)
      END DO

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE InitializeSpeciesVars(blk, nblk)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock
      USE ThermalGasVars_m
      USE SetSpeciesData_m, ONLY: SetSpeciesThermoData
      USE xml_data_input

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: nblk
      INTEGER :: iblk, ispc

      nspec = input_data%Species%nspec
      !> Allocate memories for global species data
      ALLOCATE(SPC(nspec))

      !> Read species info from input file
      DO ispc = 1, nspec
         SPC(ispc)%spcName  = TRIM(ADJUSTL(input_data%Species%species(ispc)))
         SPC(ispc)%Lewis    = input_data%Species%lewis(ispc)
         !> Temporary setup for specific heat: Here AIR property is adopted!
         SPC(ispc)%Cp       = 1010.0_wp
         SPC(ispc)%Cv       = 718.0_wp
         SPC(ispc)%R        = SPC(ispc)%Cp - SPC(ispc)%Cv
      END DO


      !> Read species data from Chemkin type input file
      CALL SetSpeciesThermoData(SPC, nspec)
      

      DO iblk = 1, nblk
         !> Allocate variables associated with every single species
         CALL AllocateSpeciesData(blk(iblk)%spc, nspec, blk(iblk)%imin, &
                                                        blk(iblk)%imax, &
                                                        blk(iblk)%jmin, &
                                                        blk(iblk)%jmax, &
                                                        blk(iblk)%kmin, &
                                                        blk(iblk)%kmax)
         !> Allocate variables associated mixture property and thermo-
         !> dynamic data
         CALL AllocateMixtureData(blk(iblk)%mix, blk(iblk)%imin, &
                                                 blk(iblk)%imax, &
                                                 blk(iblk)%jmin, &
                                                 blk(iblk)%jmax, &
                                                 blk(iblk)%kmin, &
                                                 blk(iblk)%kmax)
      END DO

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE SetInitialConditions(blk, nblk)
!-----------------------------------------------------------------------------!
      USE Parameters_m, ONLY: Ru
      USE MultiBlockVars_m, ONLY: MultiBlock
      USE xml_data_input
      USE PerfectGas_m, ONLY: EvaluateDensity
      USE ThermalGasVars_m, ONLY: SPC, nspec
      USE SetMixtureData_m, ONLY: EvaluateMWmix, UpdateMixtureThermoVariables

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: nblk
      INTEGER :: iblk, ispc
      REAL(KIND=wp) :: Rmix, MWmix, mfrSUM

      MWmix = EvaluateMWmix(input_data%Species%massfrac)
      Rmix = Ru / MWmix

      !> Check if the sum of all massfrac from read becomes unity.
      mfrSUM = 0.0_wp
      DO ispc = 1, nspec
         mfrSUM = mfrSUM + input_data%Species%massfrac(ispc)
      END DO
      IF (mfrSUM .NE. 1.0_wp) THEN
         WRITE(*,*) "----------------------------------------------------------&
                     --------------------"
         WRITE(*,*) "WARNING: Sum of all species mass fraction from input file &
                     is NOT equal to 1.0."
         WRITE(*,*) "----------------------------------------------------------&
                     --------------------"
         STOP
      END IF

      !> Initialize every block flow variables
      DO iblk = 1, nblk
         DO ispc = 1, nspec         
            blk(iblk)%spc%MassFrac(:,:,:,ispc) = input_data%Species%massfrac(ispc)
         END DO

         blk(iblk)%flow%RHO = EvaluateDensity(input_data%InitialCondition%pres, &
                                              input_data%Initialcondition%temp, &
                                              Rmix)
         blk(iblk)%flow%U   = input_data%InitialCondition%u
         blk(iblk)%flow%V   = input_data%InitialCondition%v
         blk(iblk)%flow%W   = input_data%InitialCondition%w
         !> Pressure will be evaluated from the SUBROUTINE UpdateMixtureThermo-
         !> Variables().
         !blk(iblk)%flow%P   = input_data%InitialCondition%pres
         blk(iblk)%flow%T   = input_data%Initialcondition%temp

         !> Update internal energy, enthalpy, and mixture properties.
         !> Before calling this subroutine, pressure should be first evaluated.
         CALL UpdateMixtureThermoVariables(blk(iblk), SPC, nspec)

         !> Update state vector elements with primitive variables
         CALL UpdateStateVector(blk(iblk))

      END DO

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE UpdateStateVector(blk)
!-----------------------------------------------------------------------------!
      !> Update state vector elements from primitive variables
      USE MultiBlockVars_m, ONLY: MultiBlock
      
      IMPLICIT NONE
      TYPE(MultiBlock), INTENT(INOUT) :: blk


      !> First element: RHO
      blk%flow%Q(:,:,:,1) = blk%flow%RHO(:,:,:)
      !> Second element: RHO * U
      blk%flow%Q(:,:,:,2) = blk%flow%RHO(:,:,:) * blk%flow%U(:,:,:)
      !> Third element: RHO * V
      blk%flow%Q(:,:,:,3) = blk%flow%RHO(:,:,:) * blk%flow%V(:,:,:)
      !> Fourth element: RHO * W
      blk%flow%Q(:,:,:,4) = blk%flow%RHO(:,:,:) * blk%flow%W(:,:,:)
      !> Fifth element: Total energy
      blk%flow%Q(:,:,:,5) = blk%flow%Etot(:,:,:)

   END SUBROUTINE


!-----------------------------------------------------------------------------!
   SUBROUTINE UpdatePrimitiveVariables(blk)
!-----------------------------------------------------------------------------!
      !? Update primitive variables from state vector
      USE MultiBlockVars_m, ONLY: MultiBlock
      USE SetMixtureData_m, ONLY: UpdateTempFromEnthalpy

      IMPLICIT NONE
      TYPE(MultiBlock), INTENT(INOUT) :: blk

      !> Density of mixture
      blk%flow%RHO(:,:,:)  = blk%flow%Q(:,:,:,1)
      !> U velocity
      blk%flow%U(:,:,:)    = blk%flow%Q(:,:,:,2) / blk%flow%RHO(:,:,:)
      !> V velocity
      blk%flow%V(:,:,:)    = blk%flow%Q(:,:,:,3) / blk%flow%RHO(:,:,:)
      !> W velocity
      blk%flow%W(:,:,:)    = blk%flow%Q(:,:,:,4) / blk%flow%RHO(:,:,:)
      !> Total energy
      blk%flow%Etot(:,:,:) = blk%flow%Q(:,:,:,5)

      !> Evaluate temperature from internal energy
      CALL UpdateTempFromEnthalpy(blk)

   END SUBROUTINE


!-----------------------------------------------------------------------------!
   SUBROUTINE InitializeSimulationVars(blk, nblk)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock
      USE CoordinateTransform_m, ONLY: SetGridMetrics
      USE xml_data_input
      USE FlowVariables_m, ONLY: Uref, Vref, Wref, Tref, Pref

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: nblk
      INTEGER :: iblk, norder

      !> Setup Reference value
      Uref = input_data%InitialCondition%u
      Vref = input_data%InitialCondition%v
      Wref = input_data%InitialCondition%w
      Pref = input_data%InitialCondition%pres
      Tref = input_data%Initialcondition%temp


      IF (input_data%Equations%discretization .EQ. 'FINITE_DIFFERENCE') THEN
         !> order of accuracy for spatial discretization in generalized transform
         norder = input_data%GridTransform%order
         CALL SetGridMetrics(blk, nblk, norder)
      ELSE IF(input_data%Equations%discretization .EQ. 'FINITE_VOLUME') THEN

      END IF

   END SUBROUTINE

END MODULE 
