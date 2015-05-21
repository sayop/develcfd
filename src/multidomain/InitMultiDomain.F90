!> \file: InitMultiDomain.F90
!> \author: Sayop Kim

MODULE InitMultiDomain_m
   USE Parameters_m, ONLY: wp

CONTAINS

!-----------------------------------------------------------------------------!
   SUBROUTINE InitializeMultiDomain(dom)
!-----------------------------------------------------------------------------!
      USE MultiDomainVars_m
      USE xml_data_input
      USE ReadStructuredGrid_m
      !USE MultiDomainVars_m

      IMPLICIT NONE
      TYPE(MultiDomain), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: dom
      INTEGER :: idom

      !> Initialize global variables for multidomain setup
      ndomains = input_data%MultiDomain%ndomain
      ngc      = input_data%MultiDomain%ngc
      ngls     = input_data%MultiDomain%ngls

      ALLOCATE(dom(ndomains))

      !> Read from grid file in plot3d file format
      !> Each domain is simulataneously intialized and the variables are
      !> allocated while reading the grid data.
      CALL ReadStructuredGrid(ndomains, dom)

      !> Read bc info for every surfaces surrouding the domain
      !> Will read from bcinfo.dat
      CALL ReadBCinfo(ndomains, dom)

      !> Find neighbor domains for each domain
      CALL FindNeighborDomain(ndomains, dom)

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE FindNeighborDomain(ndom, dom)
!-----------------------------------------------------------------------------!
      USE MultiDomainVars_m, ONLY: MultiDomain

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: ndom
      TYPE(MultiDomain), DIMENSION(:), INTENT(INOUT) :: dom
      INTEGER :: m

      DO m = 1, ndom
         write(*,*) dom(m)%x(1,1,1)
      END DO
   END SUBROUTINE

END MODULE
