!> \file: ReadStructuredGrid.F90
!> \author: Sayop Kim
!> \brief: Provides routines to read structured grid in plot3d file format.

MODULE PreSetup_m
   USE Parameters_m, ONLY: wp

CONTAINS

!-----------------------------------------------------------------------------!
   SUBROUTINE ReadPlot3DGrid(ndomains, dom)
!-----------------------------------------------------------------------------!
      USE MultiDomainVars_m, ONLY: MultiDomain

      IMPLICIT NONE
      TYPE(MultiDomain), DIMENSION(:), INTENT(INOUT) :: dom
      !> ndomains: number of domains read from input file
      !> ndom: number of domains read from grid file
      INTEGER, INTENT(IN) :: ndomains
      INTEGER :: ndom
      INTEGER :: i, j, k
      INTEGER :: n, m
      REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: x, y, z
      CHARACTER(LEN=128) GRIDFILE

      WRITE(*,*) '# Reading Structured grid in plot3d.'

      GRIDFILE = 'grid.dat'
      OPEN(10, FILE = GRIDFILE, FORM = "FORMATTED")
      READ(10,*) ndom
      IF (ndom .NE. ndomains) THEN
         WRITE(*,*) 'WARNING: Please check "ndomain" in input file.'
         WRITE(*,*) 'The number of domain read from grid is NOT equal to "ndomain".'
         STOP
      END IF

      !> Read domain size
      DO m = 1, ndom
         READ(10,*) dom(m)%isize, dom(m)%jsize, dom(m)%ksize
         ALLOCATE(dom(m)%x(1:dom(m)%isize, &
                           1:dom(m)%jsize, &
                           1:dom(m)%ksize))
         ALLOCATE(dom(m)%y(1:dom(m)%isize, &
                           1:dom(m)%jsize, &
                           1:dom(m)%ksize))
         ALLOCATE(dom(m)%z(1:dom(m)%isize, &
                           1:dom(m)%jsize, &
                           1:dom(m)%ksize))
      END DO

      !> Read node point coordinates
      DO m = 1, ndom
         READ(10,*) &
         (((dom(m)%x(i,j,k), i=1, dom(m)%isize), &
                             j=1, dom(m)%jsize), &
                             k=1, dom(m)%ksize), &
         (((dom(m)%y(i,j,k), i=1, dom(m)%isize), &
                             j=1, dom(m)%jsize), &
                             k=1, dom(m)%ksize), &
         (((dom(m)%z(i,j,k), i=1, dom(m)%isize), &
                             j=1, dom(m)%jsize), &
                             k=1, dom(m)%ksize)
      END DO
      CLOSE(10)

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE ReadBCinfo(ndom, dom)
!-----------------------------------------------------------------------------!
      USE MultiDomainVars_m, ONLY: MultiDomain

      IMPLICIT NONE
      TYPE(MultiDomain), DIMENSION(:), INTENT(INOUT) :: dom
      INTEGER, INTENT(IN) :: ndom
      INTEGER :: m, idom

      CHARACTER(LEN=128) BCFILE

      WRITE(*,*) '# Reading bc info...'

      BCFILE = 'bcinfo.dat'
      OPEN(10, FILE = BCFILE, FORM = "FORMATTED")
      READ(10,*) 
      DO m = 1, ndom
         READ(10,*) idom, dom(m)%bc_imin, dom(m)%bc_imax, &
                          dom(m)%bc_jmin, dom(m)%bc_jmax, &
                          dom(m)%bc_kmin, dom(m)%bc_kmax
      END DO
      CLOSE(10)

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE WriteGRID(ndom, dom)
!-----------------------------------------------------------------------------!
      USE MultiDomainVars_m, ONLY: MultiDomain

      IMPLICIT NONE
      TYPE(MultiDomain), DIMENSION(:), INTENT(IN) :: dom
      INTEGER, INTENT(IN) :: ndom
      INTEGER :: m, i, j, k

      CHARACTER(LEN=128) GRIDFILE

      WRITE(*,*) '# Writing a grid file for simulation...'
      GRIDFILE = 'GRID.DATA'
      OPEN(20, FILE = GRIDFILE, FORM = "FORMATTED")
      !> Write number of domains
      WRITE(20,*) ndom
      !> Write domain size for every domains
      DO m = 1, ndom
         WRITE(20,*) dom(m)%isize, dom(m)%jsize, dom(m)%ksize
      END DO
      !> Write node point coordinates for every domains
      DO m = 1, ndom
         WRITE(20,*) &
!         (((dom(m)%x(i,j,k), i=1, dom(m)%isize), &
!                             j=1, dom(m)%jsize), &
!                             k=1, dom(m)%ksize), &
!         (((dom(m)%y(i,j,k), i=1, dom(m)%isize), &
!                             j=1, dom(m)%jsize), &
!                             k=1, dom(m)%ksize), &
!         (((dom(m)%z(i,j,k), i=1, dom(m)%isize), &
!                             j=1, dom(m)%jsize), &
!                             k=1, dom(m)%ksize)
         
         (((dom(m)%x(i,j,k), i=1, dom(m)%isize), &
                             j=1, dom(m)%jsize), &
                             k=1, dom(m)%ksize), &
         (((dom(m)%y(i,j,k), i=1, dom(m)%isize), &
                             j=1, dom(m)%jsize), &
                             k=1, dom(m)%ksize), &
         (((dom(m)%z(i,j,k), i=1, dom(m)%isize), &
                             j=1, dom(m)%jsize), &
                             k=1, dom(m)%ksize)
      END DO
      CLOSE(20)

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE FindNeighbors(ndom, dom)
!-----------------------------------------------------------------------------!
      USE MultiDomainVars_m, ONLY: MultiDomain

      IMPLICIT NONE
      TYPE(MultiDomain), DIMENSION(:), INTENT(INOUT) :: dom
      INTEGER, INTENT(IN) :: ndom
      INTEGER :: m, n, nv, nd
      INTEGER :: i, j, k
      !> Vertices' coordinates array
      REAL(KIND=wp), DIMENSION(3,8,ndom) :: vertex
      INTEGER, DIMENSION(2:8,ndom) :: v_neighbor

      !> Collect vertices' (x,y,z) coordinates for every domains 
      DO m = 1, ndom
         !> First, loop over 8 vertices of the current domain
         n = 0
         DO i = 1, dom(m)%isize, dom(m)%isize-1
            DO j = 1, dom(m)%jsize, dom(m)%jsize-1
               DO k = 1, dom(m)%ksize, dom(m)%ksize-1
                  n = n + 1
                  vertex(1,n,m) = dom(m)%x(i,j,k)
                  vertex(2,n,m) = dom(m)%y(i,j,k)
                  vertex(3,n,m) = dom(m)%z(i,j,k)
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

      !> Find neighbors sharing 1st corner vertex
      !>> Initialize temporary array for storing 7 neighboring domains
      
      v_neighbor = VertexNeighbors()
      DO m = 1, ndom
         ! initialize neighbor matrix defined in 3x3 size: This should be done here!
         ! designed for storing neighboring domain index for the host domain.
         dom(m)%neighbor = 0
         ! v_neighbor: two elements
         ! first element for 7 neighboring domain indices based in 1st vertex point
         ! second element for domain index
         ! nv = 1: the host itself. SKIP!
         ! nv = 2: bottom
         ! nv = 3: forehead
         ! nv = 4: bottom-ahead
         ! nv = 5: left
         ! nv = 6: bottom-left
         ! nv = 7: in left-diagonal
         ! nv = 8: bottom-left-diagonal
         DO n = 1, ndom
            !> Skip identical domain
            IF (m .EQ. n) CYCLE
            DO nv = 2, 8
               IF ((vertex(1,1,m) .EQ. vertex(1,nv,n)) .AND. &
                   (vertex(2,1,m) .EQ. vertex(2,nv,n)) .AND. &
                   (vertex(3,1,m) .EQ. vertex(3,nv,n))) THEN
                  v_neighbor(nv,m) = n
                  CYCLE
               END IF
            END DO
         END DO
      END DO

!      !> Populate domain index in neighbor arrays
!      DO m = 1, ndom
!         !> Based on node point 1
!         nv = 8
!         DO i = -1, 0
!            DO j = -1, 0
!               DO k = -1, 0
!                  !> Skip the host domain: i = j = k = 0
!                  IF ((i .EQ. 0) .AND. &
!                      (j .EQ. 0) .AND. &
!                      (k .EQ. 0)) CYCLE
!                  !> Skip 0 index which indicate no neighbor domain existence
!                  IF (v_neighbor(nv,m) .NE. 0) THEN
!                     dom(m)%neighbor(i,j,k) = v_neighbor(nv,m)
!                     dom(v_neighbor(nv,m))%neighbor(-i,-j,-k) = m
!                  END IF
!                  nv = nv - 1
!               END DO
!            END DO
!         END DO 
!         !> Populate the rest of the neighbors
!         !>> Based on node point 2
!         !>> n: neighbors used below are already found above.
!         !>> here, n is the neighbor stacked above host domain, m.
!         !>> loop over nv = 7, 5, 3
!         n = dom(m)%neighbor(0,0,1)
!         k = 1
!         nv = 7
!         DO i = -1, 0
!            DO j = -1, 0
!               IF ((i .EQ. 0) .AND. &
!                   (j .EQ. 0)) CYCLE
!               IF (v_neighbor(nv,n) .NE. 0) THEN
!                  dom(m)%neighbor(i,j,k) = v_neighbor(nv,n)
!                  !dom(v_neighbor(nv,n))%neighbor(-i,-j,-k) = m
!                  write(*,*) 'sayop', v_neighbor(nv,n),n,nv
!                  dom(v_neighbor(nv,n))%neighbor(i,j,k) = m
!               END IF
!               nv = nv - 2
!            END DO
!         END DO
         !>> Based on node point 6
         !>> here, n is the neighbor on the upper right to the host, m.
         !>> loop over n = 4, 3
!         i = 1
!         j = -1
!         n = dom(m)%neighbor(1,0,1)
!         nv = 4
!         DO k = 0, 1
!            IF (v_neighbor(nv,n) .NE. 0) THEN
!               dom(m)%neighbor(i,j,k) = v_neighbor(nv,n)
!               dom(v_neighbor(nv,n))%neighbor(-i,-j,-k) = m
!            END IF
!            nv = nv - 1
!         END DO
!         !>> Based on node point 5
!         i = 1
!         k = 1
!         DO j = -1, 0
!
!         END DO
!      END DO

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   REAL(KIND=wp) FUNCTION FindVertexNeighbor()
!-----------------------------------------------------------------------------!


   END FUNCTION

!-----------------------------------------------------------------------------!
   SUBROUTINE CreateGhostLayers(ndom, dom)
!-----------------------------------------------------------------------------!
      USE MultiDomainVars_m, ONLY: MultiDomain

      IMPLICIT NONE
      TYPE(MultiDomain), DIMENSION(:), INTENT(INOUT) :: dom
      INTEGER, INTENT(IN) :: ndom
      INTEGER :: n

!      DO n = 1, ndom
!         write(*,*) n, dom(n)%neighbor(:,:,:)
!      END DO

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE WriteNODEfiles(ndom, dom)
!-----------------------------------------------------------------------------!
      USE MultiDomainVars_m, ONLY: MultiDomain

      IMPLICIT NONE
      TYPE(MultiDomain), DIMENSION(:), INTENT(IN) :: dom
      INTEGER, INTENT(IN) :: ndom
      INTEGER :: idom
      CHARACTER(LEN=128) :: FILENAME

!      DO idom = 1, ndom
!         WRITE(FILENAME,'("NODE_",I5.5,".DATA")') idom
!         
!      END DO
   END SUBROUTINE

END MODULE
