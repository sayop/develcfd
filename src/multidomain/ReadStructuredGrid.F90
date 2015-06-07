!> \file: ReadStructuredGrid.F90
!> \author: Sayop Kim
!> \brief: Provides routines to read structured grid in plot3d file format.

MODULE ReadStructuredGrid_m
   USE Parameters_m, ONLY: wp

CONTAINS

!-----------------------------------------------------------------------------!
   SUBROUTINE ReadStructuredGrid(nblk, blk)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock, ngc

      IMPLICIT NONE
      !> nblk: number of blocks read from input file
      !> nblocks: number of blocks read from grid file
      TYPE(MultiBlock), DIMENSION(:), INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: nblk
      INTEGER :: nblocks
      INTEGER :: i, j, k
      INTEGER :: n, m
      INTEGER, DIMENSION(:), ALLOCATABLE :: ni, nj, nk
      REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: x, y, z
      CHARACTER(LEN=128) :: GRIDFILE

      WRITE(*,*) 'Read Structured grid in plot3d file format'

      GRIDFILE = 'grid.dat'
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
         READ(10,*) blk(m)%iend, blk(m)%jend, blk(m)%kend
         !> Initialize block start, and end points
         blk(m)%isize = blk(m)%iend + 2*ngc
         blk(m)%jsize = blk(m)%jend + 2*ngc
         blk(m)%ksize = blk(m)%kend + 2*ngc
         blk(m)%imin = 1 - ngc
         blk(m)%jmin = 1 - ngc
         blk(m)%kmin = 1 - ngc
         blk(m)%imax = blk(m)%iend + ngc
         blk(m)%jmax = blk(m)%jend + ngc
         blk(m)%kmax = blk(m)%kend + ngc
         blk(m)%istart = 1
         blk(m)%jstart = 1
         blk(m)%kstart = 1
         ALLOCATE(blk(m)%x(blk(m)%imin:blk(m)%imax, &
                           blk(m)%jmin:blk(m)%jmax, &
                           blk(m)%kmin:blk(m)%kmax))
         ALLOCATE(blk(m)%y(blk(m)%imin:blk(m)%imax, &
                           blk(m)%jmin:blk(m)%jmax, &
                           blk(m)%kmin:blk(m)%kmax))
         ALLOCATE(blk(m)%z(blk(m)%imin:blk(m)%imax, &
                           blk(m)%jmin:blk(m)%jmax, &
                           blk(m)%kmin:blk(m)%kmax))
      END DO

      !> Read node point coordinates
      DO m = 1, nblk
         READ(10,*) &
         (((blk(m)%x(i,j,k), i=blk(m)%istart, blk(m)%iend), &
                             j=blk(m)%jstart, blk(m)%jend), &
                             k=blk(m)%kstart, blk(m)%kend), &
         (((blk(m)%y(i,j,k), i=blk(m)%istart, blk(m)%iend), &
                             j=blk(m)%jstart, blk(m)%jend), &
                             k=blk(m)%kstart, blk(m)%kend), &
         (((blk(m)%z(i,j,k), i=blk(m)%istart, blk(m)%iend), &
                             j=blk(m)%jstart, blk(m)%jend), &
                             k=blk(m)%kstart, blk(m)%kend)
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
!      DO m = 1, nblk
!         !> Allocate bc_index arrays
!         READ(10,*) iblk, blk(m)%bc_index(iblk)
!      END DO
      CLOSE(10)

   END SUBROUTINE

END MODULE
