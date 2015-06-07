!> \file: InitMultiBlock.F90
!> \author: Sayop Kim

MODULE InitMultiBlock_m
   USE Parameters_m, ONLY: wp

CONTAINS

!-----------------------------------------------------------------------------!
   SUBROUTINE InitializeMultiBlock(blk)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m
      USE xml_data_input
      USE ReadStructuredGrid_m

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: blk
      INTEGER :: iblk

      !> Initialize global variables for multiblock setup
      ndomain = input_data%MultiBlock%ndomain
      nblk    = input_data%MultiBlock%nblk
      ngc     = input_data%MultiBlock%ngc
      ngls    = input_data%MultiBlock%ngls

      ALLOCATE(blk(nblk))

      !> Read from grid file in plot3d file format
      !> Each block is simulataneously intialized and the variables are
      !> allocated while reading the grid data.
      CALL ReadStructuredGrid(nblk, blk)

      !> Read bc info for every surfaces surrouding the block
      !> Will read from bcinfo.dat
      CALL ReadBCinfo(nblk, blk)

   END SUBROUTINE


END MODULE
