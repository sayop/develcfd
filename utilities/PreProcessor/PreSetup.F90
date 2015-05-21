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
      INTEGER, DIMENSION(7,ndom) :: v_neighbor
      REAL(KIND=wp) :: distance

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
      !   | 3 +--------|---+ 7
      !   |  /         |  /
      !   | /          | /
      !   |/           |/
      ! 1 +------------+ 5

      !> Find neighbors sharing 1st corner vertex
      DO m = 1, ndom
         dom(m)%neighbor = 0
         v_neighbor = 0
         DO n = 1, ndom
            IF (m .EQ. n) CYCLE
            DO nv = 1, 8
               distance = 0_wp
               DO nd = 1, 3
                  distance = distance + vertex(nd,1,m) - vertex(nd,nv,n)
               END DO
               IF (distance .EQ. 0_wp) THEN
                  v_neighbor(nv-1,m) = n
               END IF
            END DO
         END DO
         !> Assign domain index in neighbor arrays
         nv = 8
         DO i = -1, 0
            DO j = -1, 0
               DO k = -1, 0
                  nv = nv - 1
                  !> Skip the current domain
                  IF ((i**2 + j**2 + k**2) .EQ. 0) CYCLE
                  dom(m)%neighbor(i,j,k) = v_neighbor(nv,m)
               END DO
            END DO
         END DO 
      END DO

      !> Assign domain index for the rest of neighbors.
      DO m = 1, ndom
         DO i = -1, 0
            DO j = -1, 0
               DO k = -1, 0
                  !> Skip the current domain
                  IF ((i**2 + j**2 + k**2) .EQ. 0) CYCLE
                  !> n: neighbor domain index that will receive  
                  !>    the current domain index
                  dom(n)%neighbor(-i,-j,-k) = dom(m)%neighbor(i,j,k)
               END DO
            END DO
         END DO
      END DO

   END SUBROUTINE

END MODULE
