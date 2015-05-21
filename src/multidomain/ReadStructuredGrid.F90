!> \file: ReadStructuredGrid.F90
!> \author: Sayop Kim
!> \brief: Provides routines to read structured grid in plot3d file format.

MODULE ReadStructuredGrid_m
   USE Parameters_m, ONLY: wp

CONTAINS

!-----------------------------------------------------------------------------!
   SUBROUTINE ReadStructuredGrid(ndomains, dom)
!-----------------------------------------------------------------------------!
      USE MultiDomainVars_m, ONLY: MultiDomain, ngc

      IMPLICIT NONE
      !> ndomains: number of domains read from input file
      !> ndom: number of domains read from grid file
      TYPE(MultiDomain), DIMENSION(:), INTENT(INOUT) :: dom
      INTEGER, INTENT(IN) :: ndomains
      INTEGER :: ndom
      INTEGER :: i, j, k
      INTEGER :: n, m
      INTEGER, DIMENSION(:), ALLOCATABLE :: ni, nj, nk
      REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: x, y, z
      CHARACTER(LEN=128) GRIDFILE

      WRITE(*,*) 'Read Structured grid in plot3d file format'

      GRIDFILE = 'grid.dat'
      OPEN(10, FILE = GRIDFILE, FORM = "FORMATTED")
      READ(10,*) ndom
      IF (ndom .NE. ndomains) THEN
         WRITE(*,*) 'WARNING: Please check "ndomain" in input file.' 
         WRITE(*,*) 'The number of domain read from grid is NOT equal to "ndomain".'
         STOP
      END IF

      ALLOCATE(ni(ndomains)) 
      ALLOCATE(nj(ndomains)) 
      ALLOCATE(nk(ndomains))

      !> Read domain size
      DO m = 1, ndom
         READ(10,*) dom(m)%iend, dom(m)%jend, dom(m)%kend
         !> Initialize domain start, and end points
         dom(m)%isize = dom(m)%iend + 2*ngc
         dom(m)%jsize = dom(m)%jend + 2*ngc
         dom(m)%ksize = dom(m)%kend + 2*ngc
         dom(m)%imin = 1 - ngc
         dom(m)%jmin = 1 - ngc
         dom(m)%kmin = 1 - ngc
         dom(m)%imax = dom(m)%iend + ngc
         dom(m)%jmax = dom(m)%jend + ngc
         dom(m)%kmax = dom(m)%kend + ngc
         dom(m)%istart = 1
         dom(m)%jstart = 1
         dom(m)%kstart = 1
         ALLOCATE(dom(m)%x(dom(m)%imin:dom(m)%imax, &
                           dom(m)%jmin:dom(m)%jmax, &
                           dom(m)%kmin:dom(m)%kmax))
         ALLOCATE(dom(m)%y(dom(m)%imin:dom(m)%imax, &
                           dom(m)%jmin:dom(m)%jmax, &
                           dom(m)%kmin:dom(m)%kmax))
         ALLOCATE(dom(m)%z(dom(m)%imin:dom(m)%imax, &
                           dom(m)%jmin:dom(m)%jmax, &
                           dom(m)%kmin:dom(m)%kmax))
      END DO

      !> Read node point coordinates
      DO m = 1, ndom
         READ(10,*) &
         (((dom(m)%x(i,j,k), i=dom(m)%istart, dom(m)%iend), &
                             j=dom(m)%jstart, dom(m)%jend), &
                             k=dom(m)%kstart, dom(m)%kend), &
         (((dom(m)%y(i,j,k), i=dom(m)%istart, dom(m)%iend), &
                             j=dom(m)%jstart, dom(m)%jend), &
                             k=dom(m)%kstart, dom(m)%kend), &
         (((dom(m)%z(i,j,k), i=dom(m)%istart, dom(m)%iend), &
                             j=dom(m)%jstart, dom(m)%jend), &
                             k=dom(m)%kstart, dom(m)%kend)
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

      WRITE(*,*) 'Read boundary condition info for structured grid'

      BCFILE = 'bcinfo.dat'
      OPEN(10, FILE = BCFILE, FORM = "FORMATTED")
      READ(10,*) 
      DO m = 1, ndom
         !> Allocate bc_index arrays
         READ(10,*) idom, dom(m)%bc_index(
      END DO
      CLOSE(10)

   END SUBROUTINE

END MODULE
