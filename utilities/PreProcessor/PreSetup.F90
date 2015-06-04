!> \file: ReadStructuredGrid.F90
!> \author: Sayop Kim
!> \brief: Provides routines to read structured grid in plot3d file format.

MODULE PreSetup_m
   USE Parameters_m, ONLY: wp

CONTAINS

!-----------------------------------------------------------------------------!
   SUBROUTINE ReadPlot3DGrid(ndomains, dom, ngc)
!-----------------------------------------------------------------------------!
      USE MultiDomainVars_m, ONLY: MultiDomain

      IMPLICIT NONE
      TYPE(MultiDomain), DIMENSION(:), INTENT(INOUT) :: dom
      !> ndomains: number of domains read from input file
      !> ndom: number of domains read from grid file
      INTEGER, INTENT(IN) :: ndomains, ngc
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
         !> -start, -end indicate the start and end node point 
         !> within a inner domain, excluding ghost layers.
         !> here, -start and -end variables are assigned 1 and -size, respectivcely.
         dom(m)%istart = 1
         dom(m)%jstart = 1
         dom(m)%kstart = 1
         dom(m)%iend = dom(m)%isize
         dom(m)%jend = dom(m)%jsize
         dom(m)%kend = dom(m)%ksize
         dom(m)%imin = dom(m)%istart
         dom(m)%jmin = dom(m)%jstart
         dom(m)%kmin = dom(m)%kstart
         dom(m)%imax = dom(m)%iend
         dom(m)%jmax = dom(m)%jend
         dom(m)%kmax = dom(m)%kend
         ALLOCATE(dom(m)%x(1-ngc:dom(m)%isize+ngc, &
                           1-ngc:dom(m)%jsize+ngc, &
                           1-ngc:dom(m)%ksize+ngc))
         ALLOCATE(dom(m)%y(1-ngc:dom(m)%isize+ngc, &
                           1-ngc:dom(m)%jsize+ngc, &
                           1-ngc:dom(m)%ksize+ngc))
         ALLOCATE(dom(m)%z(1-ngc:dom(m)%isize+ngc, &
                           1-ngc:dom(m)%jsize+ngc, &
                           1-ngc:dom(m)%ksize+ngc))
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
         (((dom(m)%x(i,j,k), i=dom(m)%imin, dom(m)%imax), &
                             j=dom(m)%jmin, dom(m)%jmax), &
                             k=dom(m)%kmin, dom(m)%kmax), &
         (((dom(m)%y(i,j,k), i=dom(m)%imin, dom(m)%imax), &
                             j=dom(m)%jmin, dom(m)%jmax), &
                             k=dom(m)%kmin, dom(m)%kmax), &
         (((dom(m)%z(i,j,k), i=dom(m)%imin, dom(m)%imax), &
                             j=dom(m)%jmin, dom(m)%jmax), &
                             k=dom(m)%kmin, dom(m)%kmax)
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
      INTEGER :: m, n, nv, nvv
      INTEGER :: i, j, k
      !> Vertices' coordinates array
      REAL(KIND=wp), DIMENSION(3,8,ndom) :: vertex
      !> Stores neighbor domain index based on vertex point: 1 ~ 4.
      !> Each vertex point has 8 neighbors.
      INTEGER, DIMENSION(4,8,ndom) :: v_neighbor

      WRITE(*,*) '# Finding neighbor domain info...'
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

     
      !> The way of finding neighbors of the host domain:
      !>> Find neighbor domains for each vertex: 1~4 only. 

      !> Initialize neighbor array
      DO m = 1, ndom
         ! initialize neighbor matrix defined in 3x3 size: This should be done here!
         ! designed for storing neighboring domain index for the host domain.
         dom(m)%neighbor = 0
      END DO

      !> Initialize v_neighbor array
      v_neighbor = 0
      !>> Loop over 1~4 vertices
      VertexLoop1: DO nv = 1, 4 

         !> Find the current vertex's neighbor domain indices
         DO m = 1, ndom
            DO n = 1, ndom
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

         !> Populate domain index in neighbor arrays
         SELECT CASE (nv)
         CASE (1)
            !> Based on vertex 1:
            DO m = 1, ndom
               nvv = 8
               DO i = -1, 0
                  DO j = -1, 0
                     DO k = -1, 0
                        !> Skip 0 index which indicate no neighbor domain existence
                        IF (v_neighbor(nv,nvv,m) .NE. 0) THEN
                           dom(m)%neighbor(i,j,k) = v_neighbor(nv,nvv,m)
                           dom(v_neighbor(nv,nvv,m))%neighbor(-i,-j,-k) = m
                        END IF
                        nvv = nvv - 1
                     END DO
                  END DO
               END DO
            END DO
         CASE (2)
            !> Based on vertex 2
            DO m = 1, ndom
               nvv = 8
               DO i = -1, 0
                  DO j = -1, 0
                     DO k = 0, 1
                        !> Skip 0 index which indicate no neighbor domain existence
                        !> Skip k = 0 layer because this was already updated.
                        IF (v_neighbor(nv,nvv,m) .NE. 0 .AND. k .NE. 0) THEN
                           dom(m)%neighbor(i,j,k) = v_neighbor(nv,nvv,m)
                           dom(v_neighbor(nv,nvv,m))%neighbor(-i,-j,-k) = m
                        END IF
                        nvv = nvv - 1
                     END DO
                  END DO
               END DO
            END DO
         CASE (3)
            !> Based on vertex 3
            DO m = 1, ndom
               nvv = 8
               DO i = -1, 0
                  DO j = 0, 1
                     DO k = -1, 0
                        !> Skip 0 index which indicate no neighbor domain existence
                        !> Skip j = 0 layer because this was already updated.
                        IF (v_neighbor(nv,nvv,m) .NE. 0 .AND. j .NE. 0) THEN
                           dom(m)%neighbor(i,j,k) = v_neighbor(nv,nvv,m)
                           dom(v_neighbor(nv,nvv,m))%neighbor(-i,-j,-k) = m
                        END IF
                        nvv = nvv - 1
                     END DO
                  END DO
               END DO
            END DO
         CASE (4)
            !> Based on vertex 4
            DO m = 1, ndom
               nvv = 8
               DO i = -1, 0
                  DO j = 0, 1
                     DO k = 0, 1
                        !> Skip 0 index which indicate no neighbor domain existence
                        !> Skip j = 0 layer because this was already updated.
                        IF (v_neighbor(nv,nvv,m) .NE. 0 .AND. j .NE. 0 &
                            .AND. k .NE. 0) THEN
                           dom(m)%neighbor(i,j,k) = v_neighbor(nv,nvv,m)
                           dom(v_neighbor(nv,nvv,m))%neighbor(-i,-j,-k) = m
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
   SUBROUTINE CreateGhostLayers(ndom, dom, ngc)
!-----------------------------------------------------------------------------!
      USE MultiDomainVars_m, ONLY: MultiDomain

      IMPLICIT NONE
      TYPE(MultiDomain), DIMENSION(:), INTENT(INOUT) :: dom
      INTEGER :: nstep
      INTEGER, INTENT(IN) :: ndom, ngc
      INTEGER :: m

      WRITE(*,*) '# Creating ghost layers...'

      !> Arrange node points to avoid repeated use of node points
      !> in between two domains.
      CALL ArrangeNODEpoints(ndom, dom)

      DO m = 1, ndom
         !> Define -min, and -max values with number of ghost layers
         dom(m)%imin = dom(m)%istart - ngc
         dom(m)%jmin = dom(m)%jstart - ngc
         dom(m)%kmin = dom(m)%kstart - ngc
         dom(m)%imax = dom(m)%iend + ngc
         dom(m)%jmax = dom(m)%jend + ngc
         dom(m)%kmax = dom(m)%kend + ngc
         dom(m)%isize = dom(m)%imax - dom(m)%imin + 1
         dom(m)%jsize = dom(m)%jmax - dom(m)%jmin + 1
         dom(m)%ksize = dom(m)%kmax - dom(m)%kmin + 1
      END DO

      !> Create ghost layers for each domain.
      !>> The way to create ghost layers around the domain consists
      !>> of two steps: 
      !>> 1) First create ghost layers that share the surface of the domain
      !>> 2) Next create ghost layers that share the vertices and edges.
      DO nstep = 1, 2
         CALL CreateGhostCells(ndom, dom, ngc, nstep)
      END DO

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE CreateGhostCells(ndom, dom, ngc, nstep)
!-----------------------------------------------------------------------------!
      USE MultiDomainVars_m, ONLY: MultiDomain

      IMPLICIT NONE
      TYPE(MultiDomain), DIMENSION(:), INTENT(INOUT) :: dom
      INTEGER, INTENT(IN) :: ndom, ngc, nstep
      INTEGER :: m, n, ii, jj, kk, i, j, k, l, ll
      INTEGER, DIMENSION(3) :: nn
      INTEGER :: iLoopStart, iLoopEnd, &
                 jLoopStart, jLoopEnd, &
                 kLoopStart, kLoopEnd
      INTEGER :: ibase, jbase, kbase
      INTEGER :: iRecv, jRecv, kRecv

      DomainLoop: DO m = 1, ndom
 
         iiLoop: DO ii = -1, 1
            jjLoop: DO jj = -1, 1
               kkLoop: DO kk = -1, 1

                  !> Skip the host domain
                  IF ((abs(ii)+abs(jj)+abs(kk)) .EQ. 0) CYCLE
                  !> Skip the domains that share the vertices or edges 
                  !> for the first step 
                  IF ((nstep .EQ. 1) .AND. &
                      (abs(ii)+abs(jj)+abs(kk)) .GT. 1) CYCLE
                  !> Skip the domains that share the surface
                  !> for the second step
                  IF ((nstep .EQ. 2) .AND. &
                      (abs(ii)+abs(jj)+abs(kk)) .EQ. 1) CYCLE

                  !> n: neighbor domain index
                  n = dom(m)%neighbor(ii,jj,kk)
                  !> Define ghost layer domain looping range
                  SELECT CASE(ii)
                  CASE(-1)
                     iLoopStart = dom(m)%imin
                     iLoopEnd = dom(m)%istart - 1
                     IF (n .EQ. 0) THEN
                        ibase = dom(m)%istart
                     ELSE
                        ibase = dom(n)%iend - ngc + 1
                     END IF
                  CASE(0)
                     iLoopStart = dom(m)%istart
                     iLoopEnd = dom(m)%iend
                  CASE(1)
                     iLoopStart = dom(m)%iend + 1
                     iLoopEnd = dom(m)%imax
                     IF (n .EQ. 0) THEN
                        ibase = dom(m)%iend
                     ELSE
                        ibase = dom(n)%istart
                     END IF
                  END SELECT

                  SELECT CASE(jj)
                  CASE(-1)
                     jLoopStart = dom(m)%jmin
                     jLoopEnd = dom(m)%jstart - 1
                     IF (n .EQ. 0) THEN
                        jbase = dom(m)%jstart
                     ELSE
                        jbase = dom(n)%jend - ngc + 1
                     END IF
                  CASE(0)
                     jLoopStart = dom(m)%jstart
                     jLoopEnd = dom(m)%jend
                  CASE(1)
                     jLoopStart = dom(m)%jend + 1
                     jLoopEnd = dom(m)%jmax
                     IF (n .EQ. 0) THEN
                        jbase = dom(m)%jend
                     ELSE
                        jbase = dom(n)%jstart
                     END IF
                  END SELECT

                  SELECT CASE(kk)
                  CASE(-1)
                     kLoopStart = dom(m)%kmin
                     kLoopEnd = dom(m)%kstart - 1
                     IF (n .EQ. 0) THEN
                        kbase = dom(m)%kstart
                     ELSE
                        kbase = dom(n)%kend - ngc + 1
                     END IF
                  CASE(0)
                     kLoopStart = dom(m)%kstart
                     kLoopEnd = dom(m)%kend
                  CASE(1)
                     kLoopStart = dom(m)%kend + 1
                     kLoopEnd = dom(m)%kmax
                     IF (n .EQ. 0) THEN
                        kbase = dom(m)%kend
                     ELSE
                        kbase = dom(n)%kstart
                     END IF
                  END SELECT


                  IF (n .EQ. 0) THEN
                     !> The case that neighbor doesn't exist in the pointing direction
                     SELECT CASE(nstep)
                     CASE(1)
                        !> The case with no neighbor domain. It means the domain 
                        !> boundary may be a inflow or outflow, or wall BC.
                        !> Need to extend layers based on inner points
                        DO i = iLoopStart, iLoopEnd
                           IF (ii .EQ. 0) ibase = i
                           DO j = jLoopStart, jLoopEnd
                              IF (jj .EQ. 0) jbase = j
                              DO k = kLoopStart, kLoopEnd
                                 IF (kk .EQ. 0) kbase = k
                                 dom(m)%x(i,j,k) = 2.0_wp*dom(m)%x(ibase,jbase,kbase) - &
                                   dom(m)%x(2*ibase - i, 2*jbase - j, 2*kbase - k)
                                 dom(m)%y(i,j,k) = 2.0_wp*dom(m)%y(ibase,jbase,kbase) - &
                                   dom(m)%y(2*ibase - i, 2*jbase - j, 2*kbase - k)
                                 dom(m)%z(i,j,k) = 2.0_wp*dom(m)%z(ibase,jbase,kbase) - &
                                   dom(m)%z(2*ibase - i, 2*jbase - j, 2*kbase - k)
                              END DO
                           END DO
                        END DO
                     CASE(2)
                        !> This step is to create ghost layers that are in
                        !> the vertices or edges.
                        nn = 0
                        SELECT CASE(abs(ii)+abs(jj)+abs(kk))
                        CASE(2)
                           DO l = 1, 3
                              nn(l) = 1
                              IF ((ii*nn(1) + jj*nn(2) + kk*nn(3)) .EQ. 0) THEN
                                 nn(l) = 0
                                 CYCLE
                              END IF
                              IF (dom(m)%neighbor(ii*nn(1),jj*nn(2),kk*nn(3)) .NE. 0) THEN
                                 n = dom(m)%neighbor(ii*nn(1),jj*nn(2),kk*nn(3))
                              ELSE
                                 ll = l ! Indicate the direction from the neighbor to point
                              END IF
                              nn(l) = 0
                           END DO
                           nn(ll) = 1
                           CALL CreateCornerGhostLayers(dom, m, iLoopStart, iLoopEnd, &
                                                                jLoopStart, jLoopEnd, &
                                                                kLoopStart, kLoopEnd, &
                                                                ii, jj, kk, nn, n, ngc)
                        CASE(3)
                        END SELECT
                     END SELECT
                  ELSE
                     !> The case that the domain faces with the neighbor domain.
                     !> Need to communicate with the node points in the neighbor
                     !> domain.
                     !> Extract the node point data from neighbors
                     iRecv = ibase
                     DO i = iLoopStart, iLoopEnd
                        IF (ii .EQ. 0) iRecv = i
                        jRecv = jbase
                        DO j = jLoopStart, jLoopEnd
                           IF (jj .EQ. 0) jRecv = j
                           kRecv = kbase
                           DO k = kLoopStart, kLoopEnd
                              IF (kk .EQ. 0) kRecv = k
                              dom(m)%x(i,j,k) = dom(n)%x(iRecv,jRecv,kRecv)
                              dom(m)%y(i,j,k) = dom(n)%y(iRecv,jRecv,kRecv)
                              dom(m)%z(i,j,k) = dom(n)%z(iRecv,jRecv,kRecv)
                              kRecv = kRecv + 1
                           END DO
                           jRecv = jRecv + 1
                        END DO
                        iRecv = iRecv + 1
                     END DO

                  END IF

               END DO kkLoop
            END DO jjLoop
         END DO iiLoop

      END DO DomainLoop

   END SUBROUTINE


!-----------------------------------------------------------------------------!
   SUBROUTINE CreateCornerGhostLayers(dom, m, iLoopStart, iLoopEnd, &
                                              jLoopStart, jLoopEnd, &
                                              kLoopStart, kLoopEnd, &
                                              ii, jj, kk, nn, n, ngc)
!-----------------------------------------------------------------------------!
      USE MultiDomainVars_m, ONLY: MultiDomain

      IMPLICIT NONE
      TYPE(MultiDomain), DIMENSION(:), INTENT(INOUT) :: dom
      INTEGER, DIMENSION(3), INTENT(IN) :: nn
      INTEGER, INTENT(IN) :: ngc, m, n
      INTEGER, INTENT(IN) :: iLoopStart, iLoopEnd, &
                             jLoopStart, jLoopEnd, &
                             kLoopStart, kLoopEnd
      INTEGER, INTENT(IN) :: ii, jj, kk
      INTEGER :: iRecv, jRecv, kRecv, ibase, jbase, kbase
      INTEGER :: i, j, k
!      INTEGER, DIMENSION(3), INTENT(IN) :: nn

      write(*,*) 'm=', m, nn
      IF (n .EQ. 0) RETURN
      SELECT CASE(ii)
      CASE(-1)
         ibase = dom(n)%iend - ngc + 1
      CASE(1)
         ibase = dom(n)%istart
      END SELECT

      SELECT CASE(jj)
      CASE(-1)
         jbase = dom(n)%jend - ngc + 1
      CASE(1)
         jbase = dom(n)%jstart
      END SELECT

      SELECT CASE(kk)
      CASE(-1)
         kbase = dom(n)%kend - ngc + 1
      CASE(1)
         kbase = dom(n)%kstart
      END SELECT

      iRecv = ibase
      DO i = iLoopStart, iLoopEnd
         IF ((ii .EQ. 0) .OR. (nn(1) .EQ. 1)) iRecv = i
         jRecv = jbase
         DO j = jLoopStart, jLoopEnd
            IF ((jj .EQ. 0) .OR. (nn(2) .EQ. 1)) jRecv = j
            kRecv = kbase
            DO k = kLoopStart, kLoopEnd
               IF ((kk .EQ. 0) .OR. (nn(3) .EQ. 1)) kRecv = k
               dom(m)%x(i,j,k) = dom(n)%x(iRecv,jRecv,kRecv)
               dom(m)%y(i,j,k) = dom(n)%y(iRecv,jRecv,kRecv)
               dom(m)%z(i,j,k) = dom(n)%z(iRecv,jRecv,kRecv)
               kRecv = kRecv + 1
            END DO
            jRecv = jRecv + 1
         END DO
         iRecv = iRecv + 1
      END DO

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE ArrangeNODEpoints(ndom, dom)
!-----------------------------------------------------------------------------!
      USE MultiDomainVars_m, ONLY: MultiDomain

      !>> The node point data read from the plot3d file format
      !>> contains repeated node point at the boundary surface
      !>> in between two domains. Thus, here the repeated node
      !>> points are removed to avoid redundant memory use.
      !>> -end values will be corrected.

      IMPLICIT NONE
      TYPE(MultiDomain), DIMENSION(:), INTENT(INOUT) :: dom
      INTEGER, INTENT(IN) :: ndom
      INTEGER :: m, i, j, k

      DO m = 1, ndom
         !> If there is a fluid neighbor, reduce -end by one level.
         !> If in the case, correct the domain size too.
         IF (dom(m)%neighbor(1,0,0) .NE. 0) THEN
            dom(m)%iend = dom(m)%iend - 1
            dom(m)%imax = dom(m)%imax - 1
            dom(m)%isize = dom(m)%imax - dom(m)%imin + 1
         END IF
         IF (dom(m)%neighbor(0,1,0) .NE. 0) THEN
            dom(m)%jend = dom(m)%jend - 1
            dom(m)%jmax = dom(m)%jmax - 1
            dom(m)%jsize = dom(m)%jmax - dom(m)%jmin + 1
         END IF
         IF (dom(m)%neighbor(0,0,1) .NE. 0) THEN
            dom(m)%kend = dom(m)%kend - 1
            dom(m)%kmax = dom(m)%kmax - 1
            dom(m)%ksize = dom(m)%kmax - dom(m)%kmin + 1
         END IF
      END DO

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE WriteNODEfiles(ndom, dom)
!-----------------------------------------------------------------------------!
      USE MultiDomainVars_m, ONLY: MultiDomain

      IMPLICIT NONE
      TYPE(MultiDomain), DIMENSION(:), INTENT(IN) :: dom
      INTEGER, INTENT(IN) :: ndom
      INTEGER :: m, i, j, k, neighbors
      CHARACTER(LEN=128) :: NODEFILE

      DO m = 1, ndom
         !> Find number of neighbors
         neighbors = 0
         DO i = -1, 1
            DO j = -1, 1
               DO k = -1, 1
                  !> Skip the current domain
                  IF ((i .EQ. 0) .AND. &
                      (j .EQ. 0) .AND. &
                      (k .EQ. 0)) CYCLE
                  IF (dom(m)%neighbor(i,j,k) .NE. 0) neighbors = neighbors + 1
               END DO
            END DO
         END DO
         !> Write NODE_ files
         WRITE(NODEFILE,'("NODE_",I5.5,".DATA")') m
         OPEN(30, FILE = NODEFILE, FORM = "FORMATTED")
         WRITE(30,'(A20,I6)') 'DOMAIN ID:', m
         WRITE(30,'(A20,I6)') 'NUMBER OF NEIGHBORS:', neighbors
         WRITE(30,'(6A12)') 'ISTART','IEND','JSTART','JEND','KSTART','KEND'
         WRITE(30,*) dom(m)%istart, dom(m)%iend, &
                     dom(m)%jstart, dom(m)%jend, &
                     dom(m)%kstart, dom(m)%kend
         DO i = -1, 1
            DO j = -1, 1
               DO k = -1, 1
                  !> Skip the current domain
                  IF ((i .EQ. 0) .AND. &
                      (j .EQ. 0) .AND. &
                      (k .EQ. 0)) CYCLE
                  IF (dom(m)%neighbor(i,j,k) .NE. 0) THEN
                     WRITE(30,'(A12,I6)') 'NEIGHBOR ID:', dom(m)%neighbor(i,j,k)
                     WRITE(30,'(A12,3I5)') 'LOCATION:', i, j, k
                  END IF
               END DO
            END DO
         END DO
         CLOSE(30)
      END DO
   END SUBROUTINE

END MODULE
