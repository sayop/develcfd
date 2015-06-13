!> \file: InitMultiBlock.F90
!> \author: Sayop Kim

MODULE InitMultiBlock_m
   USE Parameters_m, ONLY: wp

CONTAINS


!-----------------------------------------------------------------------------!
   SUBROUTINE ReadNODEfiles(blk, nblk, ngc)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: nblk, ngc
      INTEGER :: n, m, nneighbor, intDUMP, nID
      INTEGER :: i, j, k
      CHARACTER(LEN=128) :: NODEFILE, charDUMP

      DO m = 1, nblk
         WRITE(NODEFILE,'("NODE_",I5.5,".DATA")') m
         OPEN(30, FILE = NODEFILE, FORM = "FORMATTED")
         READ(30,'(A20,I6)') charDUMP, blk(m)%domainID
         READ(30,'(A20,I6)') charDUMP, intDUMP
         IF (m .NE. intDUMP) THEN
            WRITE(*,*) 'WARNING: blockID is incorrect in ', NODEFILE
            STOP
         END IF
         READ(30,'(A20,I6)') charDUMP, nneighbor
         READ(30,'(A20,I6)') charDUMP, intDUMP
         IF (ngc .NE. intDUMP) THEN
            WRITE(*,*) 'WARNING: ngc is incorrect in the input file!'
            STOP
         END IF
         READ(30,*) charDUMP
         READ(30,*) blk(m)%istart, blk(m)%iend, &
                    blk(m)%jstart, blk(m)%jend, &
                    blk(m)%kstart, blk(m)%kend

         !> Define block size based on the NODE files
         blk(m)%imin = blk(m)%istart - ngc
         blk(m)%imax = blk(m)%iend   + ngc
         blk(m)%jmin = blk(m)%jstart - ngc
         blk(m)%jmax = blk(m)%jend   + ngc
         blk(m)%kmin = blk(m)%kstart - ngc
         blk(m)%kmax = blk(m)%kend   + ngc

         blk(m)%isize = blk(m)%imax - blk(m)%imin + 1
         blk(m)%jsize = blk(m)%jmax - blk(m)%jmin + 1
         blk(m)%ksize = blk(m)%kmax - blk(m)%kmin + 1


         blk(m)%neighbor = 0
         DO n = 1, nneighbor
            READ(30,'(A12,I6)') charDUMP, nID
            READ(30,'(A12,3I5)') charDUMP, i, j, k
            blk(m)%neighbor(i,j,k) = nID
         END DO
         CLOSE(30)
      END DO

   END SUBROUTINE


!-----------------------------------------------------------------------------!
   SUBROUTINE ReadStructuredGrid(blk, nblk, ngc)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock, AllocateMultiBlockXYZ

      IMPLICIT NONE
      !> nblk: number of blocks read from input file
      !> nblocks: number of blocks read from grid file
      TYPE(MultiBlock), DIMENSION(:), INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: nblk, ngc
      INTEGER :: nblocks
      INTEGER :: i, j, k
      INTEGER :: isize, jsize, ksize
      INTEGER :: n, m
      INTEGER, DIMENSION(:), ALLOCATABLE :: ni, nj, nk
      REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: x, y, z
      CHARACTER(LEN=128) :: GRIDFILE

      GRIDFILE = 'GRID.DATA'
      WRITE(*,*) ''
      WRITE(*,*) '# Reading a structured grid: ', GRIDFILE

      OPEN(10, FILE = GRIDFILE, FORM = "FORMATTED")
      READ(10,*) nblocks
      IF (nblocks .NE. nblk) THEN
         WRITE(*,*) 'WARNING: Please check "nblk" in input file.'
         WRITE(*,*) 'The number of blocks read from grid is NOT equal to "nblk".'
         STOP
      END IF

      ALLOCATE(ni(nblk))
      ALLOCATE(nj(nblk))
      ALLOCATE(nk(nblk))

      !> Read block size
      DO m = 1, nblk
         READ(10,*) isize, jsize, ksize
         !> Check whether the block size read from the grid files
         !> match the sizes from NODE files
         IF ((isize .NE. blk(m)%isize) .OR. &
             (jsize .NE. blk(m)%jsize) .OR. &
             (ksize .NE. blk(m)%ksize)) THEN
            WRITE(*,*) '---------------------------------------------&
                        --------------------------------------------'
            WRITE(*,*) 'Block size from the grid file does NOT match &
                        the size from the NODE file in block ID: ', m
            WRITE(*,*) '---------------------------------------------&
                        --------------------------------------------'
            STOP
         END IF

         CALL AllocateMultiBlockXYZ(blk(m), blk(m)%imin, blk(m)%imax, &
                                            blk(m)%jmin, blk(m)%jmax, &
                                            blk(m)%kmin, blk(m)%kmax)

      END DO

      !> Read node point coordinates
      DO m = 1, nblk
         READ(10,*) &
         (((blk(m)%x(i,j,k), i=blk(m)%imin, blk(m)%imax), &
                             j=blk(m)%jmin, blk(m)%jmax), &
                             k=blk(m)%kmin, blk(m)%kmax), &
         (((blk(m)%y(i,j,k), i=blk(m)%imin, blk(m)%imax), &
                             j=blk(m)%jmin, blk(m)%jmax), &
                             k=blk(m)%kmin, blk(m)%kmax), &
         (((blk(m)%z(i,j,k), i=blk(m)%imin, blk(m)%imax), &
                             j=blk(m)%jmin, blk(m)%jmax), &
                             k=blk(m)%kmin, blk(m)%kmax)
      END DO
      CLOSE(10)

   END SUBROUTINE


!-----------------------------------------------------------------------------!
   SUBROUTINE ReadBCinfo(nblk, blk)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: nblk
      INTEGER :: m, iblk
      CHARACTER(LEN=128) BCFILE

      WRITE(*,*) 'Read boundary condition info for structured grid'

      BCFILE = 'bcinfo.dat'
      OPEN(10, FILE = BCFILE, FORM = "FORMATTED")
      READ(10,*)

      DO m = 1, nblk
         READ(10,*) iblk, blk(iblk)%bc_imin, blk(iblk)%bc_imax, &
                          blk(iblk)%bc_jmin, blk(iblk)%bc_jmax, &
                          blk(iblk)%bc_kmin, blk(iblk)%bc_kmax
      END DO
      CLOSE(10)

   END SUBROUTINE


END MODULE
