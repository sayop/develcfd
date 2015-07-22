!> \file: GlobalVars.F90
!> \author: Sayop Kim
!> \brief: Provides global simulation variables

MODULE GlobalVars_m
   USE Parameters_m, ONLY: wp

   IMPLICIT NONE
#ifndef SERIAL
   INCLUDE 'mpif.h'
#endif

   !> Number of CPUs read from input file
   INTEGER :: ncpu
   !> Corresponding processor rank
   INTEGER :: rank = 0

   !> switch for stopping simulation with errors
   INTEGER :: istop

#ifndef SERIAL
   !> block ID being used in this current CPU: SIZE = nbp
   !> elements should correspond to the global block ID
   !> Each rank will have different size and elements of this array.
   INTEGER, ALLOCATABLE, DIMENSION(:) :: blkInThisRank
   !> Rank ID assigned to each block: SIZE = nblk
   !> Used to identify the rank in which corresponding block is contained.
   INTEGER, ALLOCATABLE, DIMENSION(:) :: RankToBlk
   !> Index of array 'blkInThisRank' array for every blocks: SIZE = nblk
   INTEGER, ALLOCATABLE, DIMENSION(:) :: LocalBlkIndxInRank
#endif
   

   !> Total number of domains and  blocks
   INTEGER :: ndomain
   INTEGER :: nblk
   !> number of ghost cells being used in computation
   INTEGER :: ngc
   !> number of ghost cells being stored in restart files
   INTEGER :: ngls

END MODULE
