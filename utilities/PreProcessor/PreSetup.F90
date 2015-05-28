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
         !> -start, -end indicate the start and end node point 
         !> within a inner domain, excluding ghost layers.
         !> here, -start and -end variables are assigned 1 and -size, respectivcely.
         !> Later, -end values will be corrected depending on the neighbor presence.
         !> But, -start values will always hold 1.
         dom(m)%istart = 1
         dom(m)%jstart = 1
         dom(m)%kstart = 1
         dom(m)%iend = dom(m)%isize
         dom(m)%jend = dom(m)%jsize
         dom(m)%kend = dom(m)%ksize
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
   SUBROUTINE CreateGhostLayers(ndom, dom)
!-----------------------------------------------------------------------------!
      USE MultiDomainVars_m, ONLY: MultiDomain

      IMPLICIT NONE
      TYPE(MultiDomain), DIMENSION(:), INTENT(INOUT) :: dom
      INTEGER, INTENT(IN) :: ndom
      INTEGER :: m, nv, i, j, k

!      do m = 1, ndom
!         nv = 0
!         do i = -1, 1
!            do j = -1, 1
!               do k = -1, 1
!                  if (dom(m)%neighbor(i,j,k) .ne. 0) nv = nv + 1
!               end do
!            end do
!         end do
!         write(*,*) m, nv, dom(m)%neighbor(0,0,0)
!      end do
   END SUBROUTINE


!-----------------------------------------------------------------------------!
   SUBROUTINE ArrangeNODEpoints(ndom, dom)
!-----------------------------------------------------------------------------!
      USE MultiDomainVars_m, ONLY: MultiDomain

      IMPLICIT NONE
      TYPE(MultiDomain), DIMENSION(:), INTENT(INOUT) :: dom
      INTEGER, INTENT(IN) :: ndom
      INTEGER :: m, i, j, k

      DO m = 1, ndom
         !> If there is a fluid neighbor, reduce -end by one level.
         !> If in the case, correct the domain size too.
         IF (dom(m)%neighbor(1,0,0) .NE. 0) THEN
            dom(m)%iend = dom(m)%iend - 1
            dom(m)%isize = dom(m)%iend - dom(m)%istart + 1
         ELSE IF (dom(m)%neighbor(0,1,0) .NE. 0) THEN
            dom(m)%jend = dom(m)%jend - 1
            dom(m)%jsize = dom(m)%jend - dom(m)%jstart + 1
         ELSE IF (dom(m)%neighbor(0,0,1) .NE. 0) THEN
            dom(m)%kend = dom(m)%kend - 1
            dom(m)%ksize = dom(m)%kend - dom(m)%kstart + 1
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
                     WRITE(30,*) dom(dom(m)%neighbor(i,j,k))%iend
                     WRITE(30,*) dom(dom(m)%neighbor(i,j,k))%jend
                     WRITE(30,*) dom(dom(m)%neighbor(i,j,k))%kend
                     WRITE(30,*) ''
                  END IF
               END DO
            END DO
         END DO
         CLOSE(30)
      END DO
   END SUBROUTINE

END MODULE
