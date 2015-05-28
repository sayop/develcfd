!> \file: pre.F90
!> \author: Sayop Kim
!> \brief: Provides utility that create grid and boundary condition files

PROGRAM pre
   USE Parameters_m, ONLY: wp
   USE MultiDomainVars_m
   USE PreSetup_m
   USE io_m

   IMPLICIT NONE
   TYPE(MultiDomain), DIMENSION(:), ALLOCATABLE :: dom

   WRITE(*,*) '#############################################'
   WRITE(*,*) '### PreProcessor: GRID generation program ###'
   WRITE(*,*) '#############################################'
   !CALL WriteInputFiles()
   CALL ReadInputFiles()

   !> Initialize global variables for multidomain setup
   ndomains = input_data%MultiDomain%ndomain
   ngc      = input_data%MultiDomain%ngc

   !> Allocate multidomain variable arrays
   ALLOCATE(dom(ndomains))

   !> Read grid data for multidomain configuration in plot3d format
   CALL ReadPlot3DGrid(ndomains, dom)

   !> Read boundary condition info
   CALL ReadBCinfo(ndomains, dom)

   !> Find neighbors to each domain
   CALL FindNeighbors(ndomains, dom)

   !> Arrange node points:
   !>> The node point data read from the plot3d file format
   !>> contains repeated node point at the boundary surface
   !>> in between two domains. Thus, here the repeated node
   !>> points are removed to avoid redundant memory use.
   !>> -end values will be corrected.
   CALL ArrangeNODEpoints(ndomains, dom)

   !> Create ghost-layers with ngc
   CALL CreateGhostLayers(ndomains, dom)

   !> Write NODE files
   CALL WriteNODEfiles(ndomains, dom)

   !> Write a grid file for simulation
   CALL WriteGRID(ndomains, dom)
END PROGRAM pre
