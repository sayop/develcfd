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
   CALL ReadPlot3DGrid(ndomains, dom, ngc)

   !> Read boundary condition info
   CALL ReadBCinfo(ndomains, dom)

   !> Find neighbors to each domain
   CALL FindNeighbors(ndomains, dom)

   !> Create ghost-layers with ngc
   CALL CreateGhostLayers(ndomains, dom, ngc)

   !> Write NODE files
   CALL WriteNODEfiles(ndomains, dom)

   !> Write a grid file for simulation
   CALL WriteGRID(ndomains, dom)
END PROGRAM pre
