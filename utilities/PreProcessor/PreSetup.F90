!> \file: PreSetup.F90
!> \author: Sayop Kim
!> \brief: Provides routines to read structured grid in plot3d file format.

MODULE PreSetup_m
   USE Parameters_m, ONLY: wp

CONTAINS

!-----------------------------------------------------------------------------!
   SUBROUTINE ReadPlot3DGrid(ndomain, nblk, dom, blk, ngc)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock, MultiDomain
      USE AllocateVars_m, ONLY: AllocateMultiBlockXYZ

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(INOUT) :: blk
      TYPE(MultiDomain), DIMENSION(:), INTENT(INOUT) :: dom
      !> nblk: number of blocks read from input file
      !> nblock: number of blocks read from grid file
      INTEGER, INTENT(IN) :: ndomain, nblk, ngc
      INTEGER :: nblock
      INTEGER :: i, j, k
      INTEGER :: n, m
      REAL(KIND=wp), DIMENSION(:,:,:,:), ALLOCATABLE :: x, y, z
      CHARACTER(LEN=128) :: GRIDFILE

      GRIDFILE = 'grid.dat'
      WRITE(*,*) ''
      WRITE(*,*) '# Reading PLOT3D structured grid file: ', GRIDFILE

      OPEN(10, FILE = GRIDFILE, FORM = "FORMATTED")
      READ(10,*) nblock
      IF (nblock .NE. nblk) THEN
         WRITE(*,*) '-----------------------------------------------------------'
         WRITE(*,*) 'WARNING: Please check "nblk" in input file.'
         WRITE(*,*) 'The number of blocks read from grid is NOT equal to "nblk".'
         WRITE(*,*) '-----------------------------------------------------------'
         STOP
      END IF

      IF (ndomain .NE. nblk) THEN
         WRITE(*,*) '--------------------------------------------------------------'
         WRITE(*,*) 'WARNING: Please check "ndomain" and "nblk" in input file.'
         WRITE(*,*) 'The number of domains should be same as the number of blocks.'
         WRITE(*,*) '--------------------------------------------------------------'
         STOP
      END IF

      !> Read block size
      DO m = 1, nblock
         READ(10,*) blk(m)%isize, blk(m)%jsize, blk(m)%ksize
         !> -start, -end indicate the start and end node point 
         !> within a inner block, excluding ghost layers.
         !> here, -start and -end variables are assigned 1 and -size, respectivcely.
         blk(m)%istart = 1
         blk(m)%jstart = 1
         blk(m)%kstart = 1
         blk(m)%iend = blk(m)%isize
         blk(m)%jend = blk(m)%jsize
         blk(m)%kend = blk(m)%ksize
         blk(m)%imin = blk(m)%istart
         blk(m)%jmin = blk(m)%jstart
         blk(m)%kmin = blk(m)%kstart
         blk(m)%imax = blk(m)%iend
         blk(m)%jmax = blk(m)%jend
         blk(m)%kmax = blk(m)%kend

         CALL AllocateMultiBlockXYZ(blk, m, 1-ngc, blk(m)%isize+ngc, &
                                            1-ngc, blk(m)%jsize+ngc, &
                                            1-ngc, blk(m)%ksize+ngc)
         blk(m)%domainID = m

         !> Initialize domain info
         dom(m)%istart = 1
         dom(m)%jstart = 1
         dom(m)%kstart = 1
         dom(m)%iend = blk(m)%iend
         dom(m)%jend = blk(m)%jend
         dom(m)%kend = blk(m)%kend
         dom(m)%nblocks = 1
         ALLOCATE(dom(m)%blockID(1))
         dom(m)%blockID(1) = m
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

      WRITE(*,*) ''
      WRITE(*,*) '# Reading bc info...'

      BCFILE = 'bcinfo.dat'
      OPEN(10, FILE = BCFILE, FORM = "FORMATTED")
      READ(10,*) 
      DO m = 1, nblk
         READ(10,*) iblk, blk(m)%bc_imin, blk(m)%bc_imax, &
                          blk(m)%bc_jmin, blk(m)%bc_jmax, &
                          blk(m)%bc_kmin, blk(m)%bc_kmax
      END DO
      CLOSE(10)

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE WriteGRID(nblk, blk, GRIDFILE)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(IN) :: blk
      INTEGER, INTENT(IN) :: nblk
      INTEGER :: m, i, j, k

      CHARACTER(LEN=128), INTENT(IN) :: GRIDFILE

      WRITE(*,*) ''
      WRITE(*,*) '# Writing a grid file: ', GRIDFILE

      OPEN(20, FILE = GRIDFILE, FORM = "FORMATTED")
      !> Write number of blocks
      WRITE(20,*) nblk
      !> Write block size for every blocks
      DO m = 1, nblk
         WRITE(20,*) blk(m)%isize, blk(m)%jsize, blk(m)%ksize
      END DO
      !> Write node point coordinates for every blocks
      DO m = 1, nblk
         WRITE(20,*) &
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
      CLOSE(20)

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE FindNeighbors(nblk, blk)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: nblk
      INTEGER :: m, n, nv, nvv
      INTEGER :: i, j, k
      !> Vertices' coordinates array
      REAL(KIND=wp), DIMENSION(3,8,nblk) :: vertex
      !> Stores neighbor block index based on vertex point: 1 ~ 4.
      !> Each vertex point has 8 neighbors.
      INTEGER, DIMENSION(4,8,nblk) :: v_neighbor

      WRITE(*,*) ''
      WRITE(*,*) '# Finding neighbor block info...'
      !> Collect vertices' (x,y,z) coordinates for every blocks 
      DO m = 1, nblk
         !> First, loop over 8 vertices of the current block
         n = 0
         DO i = blk(m)%istart, blk(m)%iend, blk(m)%isize-1
            DO j = blk(m)%jstart, blk(m)%jend, blk(m)%jsize-1
               DO k = blk(m)%kstart, blk(m)%kend, blk(m)%ksize-1
                  n = n + 1
                  vertex(1,n,m) = blk(m)%x(i,j,k)
                  vertex(2,n,m) = blk(m)%y(i,j,k)
                  vertex(3,n,m) = blk(m)%z(i,j,k)
               END DO
            END DO
         END DO
      END DO


      !  Vertex index:
      !     4 +------------+ 8
      !      /|           /|
      !     / |          / |
      !    /  |         /  |
      ! 2 +------------+ 6 |
      !   |   |        |   |
      !   | 3 +--------|---+ 7        z
      !   |  /         |  /           ^  y
      !   | /          | /            | /
      !   |/           |/             |/
      ! 1 +------------+ 5            +--> x
      !
      ! vertex index, n, is a second element of vertex(:,:,:) array
      ! first element: x, y, z indicator
      ! third element: host, neighbors indicator 

     
      !> The way of finding neighbors of the host block:
      !>> Find neighbor blocks for each vertex: 1~4 only. 

      !> Initialize neighbor array
      DO m = 1, nblk
         ! initialize neighbor matrix defined in 3x3 size: This should be done here!
         ! designed for storing neighboring block index for the host block.
         blk(m)%neighbor = 0
      END DO

      !> Initialize v_neighbor array
      v_neighbor = 0
      !>> Loop over 1~4 vertices
      VertexLoop1: DO nv = 1, 4 

         !> Find the current vertex's neighbor block indices
         DO m = 1, nblk
            DO n = 1, nblk
               DO nvv = 1, 8
                  IF ((vertex(1,nv,m) .EQ. vertex(1,nvv,n)) .AND. &
                      (vertex(2,nv,m) .EQ. vertex(2,nvv,n)) .AND. &
                      (vertex(3,nv,m) .EQ. vertex(3,nvv,n))) THEN
                     v_neighbor(nv,nvv,m) = n
                     CYCLE
                  END IF
               END DO
            END DO
         END DO

         !> Populate block index in neighbor arrays
         SELECT CASE (nv)
         CASE (1)
            !> Based on vertex 1:
            DO m = 1, nblk
               nvv = 8
               DO i = -1, 0
                  DO j = -1, 0
                     DO k = -1, 0
                        !> Skip 0 index which indicate no neighbor block existence
                        IF (v_neighbor(nv,nvv,m) .NE. 0) THEN
                           blk(m)%neighbor(i,j,k) = v_neighbor(nv,nvv,m)
                           blk(v_neighbor(nv,nvv,m))%neighbor(-i,-j,-k) = m
                        END IF
                        nvv = nvv - 1
                     END DO
                  END DO
               END DO
            END DO
         CASE (2)
            !> Based on vertex 2
            DO m = 1, nblk
               nvv = 8
               DO i = -1, 0
                  DO j = -1, 0
                     DO k = 0, 1
                        !> Skip 0 index which indicate no neighbor block existence
                        !> Skip k = 0 layer because this was already updated.
                        IF (v_neighbor(nv,nvv,m) .NE. 0 .AND. k .NE. 0) THEN
                           blk(m)%neighbor(i,j,k) = v_neighbor(nv,nvv,m)
                           blk(v_neighbor(nv,nvv,m))%neighbor(-i,-j,-k) = m
                        END IF
                        nvv = nvv - 1
                     END DO
                  END DO
               END DO
            END DO
         CASE (3)
            !> Based on vertex 3
            DO m = 1, nblk
               nvv = 8
               DO i = -1, 0
                  DO j = 0, 1
                     DO k = -1, 0
                        !> Skip 0 index which indicate no neighbor block existence
                        !> Skip j = 0 layer because this was already updated.
                        IF (v_neighbor(nv,nvv,m) .NE. 0 .AND. j .NE. 0) THEN
                           blk(m)%neighbor(i,j,k) = v_neighbor(nv,nvv,m)
                           blk(v_neighbor(nv,nvv,m))%neighbor(-i,-j,-k) = m
                        END IF
                        nvv = nvv - 1
                     END DO
                  END DO
               END DO
            END DO
         CASE (4)
            !> Based on vertex 4
            DO m = 1, nblk
               nvv = 8
               DO i = -1, 0
                  DO j = 0, 1
                     DO k = 0, 1
                        !> Skip 0 index which indicate no neighbor block existence
                        !> Skip j = 0 layer because this was already updated.
                        IF (v_neighbor(nv,nvv,m) .NE. 0 .AND. j .NE. 0 &
                            .AND. k .NE. 0) THEN
                           blk(m)%neighbor(i,j,k) = v_neighbor(nv,nvv,m)
                           blk(v_neighbor(nv,nvv,m))%neighbor(-i,-j,-k) = m
                        END IF
                        nvv = nvv - 1
                     END DO
                  END DO
               END DO
            END DO
         END SELECT
         
      END DO VertexLoop1

   END SUBROUTINE


!-----------------------------------------------------------------------------!
   SUBROUTINE CreateGhostLayers(nblk, blk, ngc)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(INOUT) :: blk
      INTEGER :: nstep
      INTEGER, INTENT(IN) :: nblk, ngc
      INTEGER :: m

      WRITE(*,*) ''
      WRITE(*,*) '# Creating ghost layers...'

      !> Arrange node points to avoid repeated use of node points
      !> in between two blocks.
      CALL ArrangeNODEpoints(nblk, blk)

      DO m = 1, nblk
         !> Define -min, and -max values with number of ghost layers
         blk(m)%imin = blk(m)%istart - ngc
         blk(m)%jmin = blk(m)%jstart - ngc
         blk(m)%kmin = blk(m)%kstart - ngc
         blk(m)%imax = blk(m)%iend + ngc
         blk(m)%jmax = blk(m)%jend + ngc
         blk(m)%kmax = blk(m)%kend + ngc
         blk(m)%isize = blk(m)%imax - blk(m)%imin + 1
         blk(m)%jsize = blk(m)%jmax - blk(m)%jmin + 1
         blk(m)%ksize = blk(m)%kmax - blk(m)%kmin + 1
      END DO

      !> Create ghost layers for each block.
      !>> The way to create ghost layers around the block consists
      !>> of two steps: 
      !>> 1) First create ghost layers that share the surface of the block
      !>> 2) Next create ghost layers that share the vertices and edges.
      DO nstep = 1, 2
         CALL CreateGhostCells(nblk, blk, ngc, nstep)
      END DO

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE CreateGhostCells(nblk, blk, ngc, nstep)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: nblk, ngc, nstep
      INTEGER :: m, n, ii, jj, kk, i, j, k, l, ll, iFound
      INTEGER, DIMENSION(3) :: nn
      INTEGER :: iLoopStart, iLoopEnd, &
                 jLoopStart, jLoopEnd, &
                 kLoopStart, kLoopEnd
      INTEGER :: ibase, jbase, kbase
      INTEGER :: iRecv, jRecv, kRecv

      BlockLoop: DO m = 1, nblk
 
         iiLoop: DO ii = -1, 1
            jjLoop: DO jj = -1, 1
               kkLoop: DO kk = -1, 1

                  !> Skip the host block
                  IF ((abs(ii)+abs(jj)+abs(kk)) .EQ. 0) CYCLE
                  !> Skip the blocks that share the vertices or edges 
                  !> for the first step 
                  IF ((nstep .EQ. 1) .AND. &
                      (abs(ii)+abs(jj)+abs(kk)) .GT. 1) CYCLE
                  !> Skip the blocks that share the surface
                  !> for the second step
                  IF ((nstep .EQ. 2) .AND. &
                      (abs(ii)+abs(jj)+abs(kk)) .EQ. 1) CYCLE

                  !> n: neighbor block index
                  n = blk(m)%neighbor(ii,jj,kk)
                  !> Define ghost layer block looping range
                  CALL FindGhostLayerIndices(blk, m, n, ngc, ii, jj, kk, &
                                             iLoopStart, iLoopEnd, &
                                             jLoopStart, jLoopEnd, &
                                             kLoopStart, kLoopEnd, &
                                             ibase, jbase, kbase)


                  SELECT CASE(nstep)
                  !> STEP 1: Loop for creating ghost layers that can be generated
                  !>         by extension from innter points and neighbor blocks
                  CASE(1)
                     IF (n .EQ. 0) THEN
                        !> The case with no neighbor block. It means the block 
                        !> boundary may be a inflow or outflow, or wall BC.
                        !> Need to extend layers based on inner points
                        CALL CreateGhostLayerFromInnerPoints(blk, m, ii, jj, kk, &
                                                             iLoopStart, iLoopEnd, &
                                                             jLoopStart, jLoopEnd, &
                                                             kLoopStart, kLoopEnd, &
                                                             ibase, jbase, kbase)
                     ELSE
                        !> The case that the block faces with the neighbor block.
                        !> Need to communicate with the node points in the neighbor
                        !> block.
                        !> Extract the node point data from neighbors
                        CALL CreateGhostLayerFromNeighbor(blk, m, n, ii, jj, kk, &
                                                          iLoopStart, iLoopEnd, &
                                                          jLoopStart, jLoopEnd, &
                                                          kLoopStart, kLoopEnd, &
                                                          ibase, jbase, kbase)
                     END IF
                  !> STEP 2: Loop for creating ghost layers located in the corner 
                  !>         of vertices or edges. 
                  !>         There are two cases: 
                  !>         (1) From the neighbors 
                  !>         (2) From the corresponding block's ghost layers 
                  !>             that has been generated in STEP1
                  CASE(2)
                     IF (n .NE. 0) THEN
                        !> STEP2-(1)
                        !> This is the case that ghost layer overlaps with a neighbor
                        !> Create the layers from the neighbor block node points
                        CALL CreateGhostLayerFromNeighbor(blk, m, n, ii, jj, kk, &
                                                          iLoopStart, iLoopEnd, &
                                                          jLoopStart, jLoopEnd, &
                                                          kLoopStart, kLoopEnd, &
                                                          ibase, jbase, kbase)
                     ELSE
                        !> STEP2-(2)
                        CALL CreateCornerGhostLayers(blk, m, ngc, ii, jj, kk, &
                                                     iLoopstart, iLoopEnd, &
                                                     jLoopstart, jLoopEnd, &
                                                     kLoopstart, kLoopEnd, &
                                                     ibase, jbase, kbase)
                     END IF
                  END SELECT

               END DO kkLoop
            END DO jjLoop
         END DO iiLoop

      END DO BlockLoop

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE CreateCornerGhostLayers(blk, m, ngc, ii, jj, kk, &
                                              iLoopStart, iLoopEnd, &
                                              jLoopStart, jLoopEnd, &
                                              kLoopStart, kLoopEnd, &
                                              ibase, jbase, kbase)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: ngc, m
      INTEGER, INTENT(IN) :: iLoopStart, iLoopEnd, &
                             jLoopStart, jLoopEnd, &
                             kLoopStart, kLoopEnd
      INTEGER, INTENT(IN) :: ii, jj, kk
      INTEGER :: i, j, k, ibase, jbase, kbase

      DO i = iLoopStart, iLoopEnd
         IF (ii .EQ. 0) ibase = i
         DO j = jLoopStart, jLoopEnd
            IF (jj .EQ. 0) jbase = j
            DO k = kLoopStart, kLoopEnd
               IF (kk .EQ. 0) kbase = k
               blk(m)%x(i,j,k) = -2*blk(m)%x(ibase,jbase,kbase) + &
                                 blk(m)%x(i,jbase,kbase) + &
                                 blk(m)%x(ibase,j,kbase) + &
                                 blk(m)%x(ibase,jbase,k)
               blk(m)%y(i,j,k) = -2*blk(m)%y(ibase,jbase,kbase) + &
                                 blk(m)%y(i,jbase,kbase) + &
                                 blk(m)%y(ibase,j,kbase) + &
                                 blk(m)%y(ibase,jbase,k)
               blk(m)%z(i,j,k) = -2*blk(m)%z(ibase,jbase,kbase) + &
                                 blk(m)%z(i,jbase,kbase) + &
                                 blk(m)%z(ibase,j,kbase) + &
                                 blk(m)%z(ibase,jbase,k)
            END DO
         END DO
      END DO
   END SUBROUTINE

   
!-----------------------------------------------------------------------------!
   SUBROUTINE CreateGhostLayerFromInnerPoints(blk, m, ii, jj, kk, &
                                              iLoopStart, iLoopEnd, &
                                              jLoopStart, jLoopEnd, &
                                              kLoopStart, kLoopEnd, &
                                              ibase, jbase, kbase)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: m, ii, jj, kk, iLoopStart, iLoopEnd, &
                                            jLoopStart, jLoopEnd, &
                                            kLoopStart, kLoopEnd
      INTEGER :: i, j, k, ibase, jbase, kbase

      DO i = iLoopStart, iLoopEnd
         IF (ii .EQ. 0) ibase = i
         DO j = jLoopStart, jLoopEnd
            IF (jj .EQ. 0) jbase = j
            DO k = kLoopStart, kLoopEnd
               IF (kk .EQ. 0) kbase = k
               blk(m)%x(i,j,k) = 2.0_wp*blk(m)%x(ibase,jbase,kbase) - &
                        blk(m)%x(2*ibase - i, 2*jbase - j, 2*kbase - k)
               blk(m)%y(i,j,k) = 2.0_wp*blk(m)%y(ibase,jbase,kbase) - &
                        blk(m)%y(2*ibase - i, 2*jbase - j, 2*kbase - k)
               blk(m)%z(i,j,k) = 2.0_wp*blk(m)%z(ibase,jbase,kbase) - &
                        blk(m)%z(2*ibase - i, 2*jbase - j, 2*kbase - k)
            END DO
         END DO
      END DO

   END SUBROUTINE


!-----------------------------------------------------------------------------!
   SUBROUTINE CreateGhostLayerFromNeighbor(blk, m, n, ii, jj, kk, &
                                           iLoopStart, iLoopEnd, &
                                           jLoopStart, jLoopEnd, &
                                           kLoopStart, kLoopEnd, &
                                           ibase, jbase, kbase)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: m, n, ii, jj, kk, iLoopStart, iLoopEnd, &
                                               jLoopStart, jLoopEnd, &
                                               kLoopStart, kLoopEnd
      INTEGER :: i, j, k, ibase, jbase, kbase, &
                 iRecv, jRecv, kRecv

      iRecv = ibase
      DO i = iLoopStart, iLoopEnd
         IF (ii .EQ. 0) iRecv = i
         jRecv = jbase
         DO j = jLoopStart, jLoopEnd
            IF (jj .EQ. 0) jRecv = j
            kRecv = kbase
            DO k = kLoopStart, kLoopEnd
               IF (kk .EQ. 0) kRecv = k
               blk(m)%x(i,j,k) = blk(n)%x(iRecv,jRecv,kRecv)
               blk(m)%y(i,j,k) = blk(n)%y(iRecv,jRecv,kRecv)
               blk(m)%z(i,j,k) = blk(n)%z(iRecv,jRecv,kRecv)
               kRecv = kRecv + 1
            END DO
            jRecv = jRecv + 1
         END DO
         iRecv = iRecv + 1
      END DO

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE FindGhostLayerIndices(blk, m, n, ngc, ii, jj, kk, &
                                    iLoopStart, iLoopEnd, &
                                    jLoopStart, jLoopEnd, &
                                    kLoopStart, kLoopEnd, &
                                    ibase, jbase, kbase)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(IN) :: blk
      INTEGER, INTENT(OUT) :: iLoopStart, iLoopEnd, &
                              jLoopStart, jLoopEnd, &
                              kLoopStart, kLoopEnd, &
                              ibase, jbase, kbase
      INTEGER, INTENT(IN) :: m, n, ngc, ii, jj, kk

      SELECT CASE(ii)
      CASE(-1)
         iLoopStart = blk(m)%imin
         iLoopEnd = blk(m)%istart - 1
         IF (n .EQ. 0) THEN
            ibase = blk(m)%istart
         ELSE
            ibase = blk(n)%iend - ngc + 1
         END IF
      CASE(0)
         iLoopStart = blk(m)%istart
         iLoopEnd = blk(m)%iend
      CASE(1)
         iLoopStart = blk(m)%iend + 1
         iLoopEnd = blk(m)%imax
         IF (n .EQ. 0) THEN
            ibase = blk(m)%iend
         ELSE
            ibase = blk(n)%istart
         END IF
      END SELECT

      SELECT CASE(jj)
      CASE(-1)
         jLoopStart = blk(m)%jmin
         jLoopEnd = blk(m)%jstart - 1
         IF (n .EQ. 0) THEN
            jbase = blk(m)%jstart
         ELSE
            jbase = blk(n)%jend - ngc + 1
         END IF
      CASE(0)
         jLoopStart = blk(m)%jstart
         jLoopEnd = blk(m)%jend
      CASE(1)
         jLoopStart = blk(m)%jend + 1
         jLoopEnd = blk(m)%jmax
         IF (n .EQ. 0) THEN
            jbase = blk(m)%jend
         ELSE
            jbase = blk(n)%jstart
         END IF
      END SELECT

      SELECT CASE(kk)
      CASE(-1)
         kLoopStart = blk(m)%kmin
         kLoopEnd = blk(m)%kstart - 1
         IF (n .EQ. 0) THEN
            kbase = blk(m)%kstart
         ELSE
            kbase = blk(n)%kend - ngc + 1
         END IF
      CASE(0)
         kLoopStart = blk(m)%kstart
         kLoopEnd = blk(m)%kend
      CASE(1)
         kLoopStart = blk(m)%kend + 1
         kLoopEnd = blk(m)%kmax
         IF (n .EQ. 0) THEN
            kbase = blk(m)%kend
         ELSE
            kbase = blk(n)%kstart
         END IF
      END SELECT

   END SUBROUTINE


!-----------------------------------------------------------------------------!
   SUBROUTINE ArrangeNODEpoints(nblk, blk)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock

      !>> The node point data read from the plot3d file format
      !>> contains repeated node point at the boundary surface
      !>> in between two blocks. Thus, here the repeated node
      !>> points are removed to avoid redundant memory use.
      !>> -end values will be corrected.

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: nblk
      INTEGER :: m, i, j, k

      DO m = 1, nblk
         !> If there is a fluid neighbor, reduce -end by one level.
         !> If in the case, correct the block size too.
         IF (blk(m)%neighbor(1,0,0) .NE. 0) THEN
            blk(m)%iend = blk(m)%iend - 1
            blk(m)%imax = blk(m)%imax - 1
            blk(m)%isize = blk(m)%imax - blk(m)%imin + 1
         END IF
         IF (blk(m)%neighbor(0,1,0) .NE. 0) THEN
            blk(m)%jend = blk(m)%jend - 1
            blk(m)%jmax = blk(m)%jmax - 1
            blk(m)%jsize = blk(m)%jmax - blk(m)%jmin + 1
         END IF
         IF (blk(m)%neighbor(0,0,1) .NE. 0) THEN
            blk(m)%kend = blk(m)%kend - 1
            blk(m)%kmax = blk(m)%kmax - 1
            blk(m)%ksize = blk(m)%kmax - blk(m)%kmin + 1
         END IF
      END DO

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE WriteNODEfiles(nblk, blk, ngc)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(IN) :: blk
      INTEGER, INTENT(IN) :: nblk, ngc
      INTEGER :: m, i, j, k, neighbors
      CHARACTER(LEN=128) :: NODEFILE

      WRITE(*,*) ''
      WRITE(*,*) '# Writing NODE Files...'

      DO m = 1, nblk
         !> Find number of neighbors
         neighbors = 0
         DO i = -1, 1
            DO j = -1, 1
               DO k = -1, 1
                  !> Skip the current block
                  IF ((i .EQ. 0) .AND. &
                      (j .EQ. 0) .AND. &
                      (k .EQ. 0)) CYCLE
                  IF (blk(m)%neighbor(i,j,k) .NE. 0) neighbors = neighbors + 1
               END DO
            END DO
         END DO
         !> Write NODE_ files
         WRITE(NODEFILE,'("NODE_",I5.5,".DATA")') m
         OPEN(30, FILE = NODEFILE, FORM = "FORMATTED")
         WRITE(30,'(A20,I6)') 'DOMAIN ID:', blk(m)%domainID
         WRITE(30,'(A20,I6)') 'BLOCK ID:', m
         WRITE(30,'(A20,I6)') 'NUMBER OF NEIGHBORS:', neighbors
         WRITE(30,'(A20,I6)') 'NUMBER OF LEVELS:', ngc
         WRITE(30,'(6A12)') 'ISTART','IEND','JSTART','JEND','KSTART','KEND'
         WRITE(30,*) blk(m)%istart, blk(m)%iend, &
                     blk(m)%jstart, blk(m)%jend, &
                     blk(m)%kstart, blk(m)%kend
         DO i = -1, 1
            DO j = -1, 1
               DO k = -1, 1
                  !> Skip the current block
                  IF ((i .EQ. 0) .AND. &
                      (j .EQ. 0) .AND. &
                      (k .EQ. 0)) CYCLE
                  IF (blk(m)%neighbor(i,j,k) .NE. 0) THEN
                     WRITE(30,'(A12,I6)') 'NEIGHBOR ID:', blk(m)%neighbor(i,j,k)
                     WRITE(30,'(A12,3I5)') 'LOCATION:', i, j, k
                  END IF
               END DO
            END DO
         END DO
         CLOSE(30)
      END DO
   END SUBROUTINE

END MODULE
