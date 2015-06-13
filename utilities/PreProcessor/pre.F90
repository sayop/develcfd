!> \file: pre.F90
!> \author: Sayop Kim
!> \brief: Provides utility that create grid and boundary condition files

PROGRAM pre
   USE Parameters_m, ONLY: wp
   USE MultiBlockVars_m
   USE PreSetup_m
   USE CreateGrid_m
   USE io_m

   IMPLICIT NONE
   TYPE(MultiBlock), DIMENSION(:), ALLOCATABLE :: blk
   TYPE(MultiDomain), DIMENSION(:), ALLOCATABLE :: dom
   CHARACTER(LEN=128) :: GRIDFILE

   WRITE(*,*) '#############################################'
   WRITE(*,*) '### PreProcessor: GRID generation program ###'
   WRITE(*,*) '#############################################'

   CALL ReadInputFiles()

!   CALL WriteInputFiles()
   !> Initialize global variables for multiblock setup
   ndomain = input_data%MultiBlock%ndomain
   nblk    = input_data%MultiBlock%nblk
   ngc     = input_data%MultiBlock%ngc

   !> Allocate multidomain/multiblock variable arrays
   ALLOCATE(dom(ndomain))
   ALLOCATE(blk(nblk))

#ifdef CREATE_GRID
   !> Create 1domain grid
   CALL Create1DomainGrid(ndomain, nblk, dom, blk, ngc)

#else
   !> Read grid data for multidomain configuration in plot3d format
   CALL ReadPlot3DGrid(ndomain, nblk, dom, blk, ngc)
#endif

   !> Read boundary condition info
   !CALL ReadBCinfo(nblk, blk)

   !> Find neighbors to each block
   CALL FindNeighbors(nblk, blk)

   !> Create ghost-layers with ngc
   CALL CreateGhostLayers(nblk, blk, ngc)

   !> Write NODE files
   CALL WriteNODEfiles(nblk, blk, ngc)

   !> Write a grid file for simulation
   GRIDFILE = 'GRID.DATA'
   CALL WriteGRID(nblk, blk, GRIDFILE)
END PROGRAM pre
