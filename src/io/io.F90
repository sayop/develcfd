!> \file: io.F90
!> \author: Sayop Kim
!> \brief: Provides routines to read input and write output

MODULE io_m
   USE Parameters_m, ONLY: wp
   USE xml_data_input

   INTEGER, PARAMETER :: filenameLength = 64
CONTAINS

!-----------------------------------------------------------------------------!
   SUBROUTINE ReadInputFiles()
!-----------------------------------------------------------------------------!
      USE GlobalVars_m, ONLY: ndomain, nblk, ngc, ngls, ncpu

      IMPLICIT NONE
      CHARACTER(LEN=filenameLength) :: filename

      filename = 'cfd_input.xml'

      CALL read_xml_file_input(filename)

      !> Initialize global variables for multiblock setup
      ndomain = input_data%MultiBlock%ndomain
      nblk    = input_data%MultiBlock%nblk
      ncpu    = input_data%MultiBlock%ncpu
      ngc     = input_data%MultiBlock%ngc
      ngls    = input_data%MultiBlock%ngls

   END SUBROUTINE ReadInputFiles

!-----------------------------------------------------------------------------!
   SUBROUTINE WriteInputFiles()
!-----------------------------------------------------------------------------!

     IMPLICIT NONE
     CHARACTER(LEN=filenameLength) :: filename

     filename = 'new_input.xml'
     CALL write_xml_file_input(filename)

   END SUBROUTINE WriteInputFiles

!-----------------------------------------------------------------------------!
   SUBROUTINE WriteRESTfile(blk,  nblk)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(IN) :: blk
      INTEGER, INTENT(IN) :: nblk
      INTEGER :: iblk, i, j, k
      INTEGER :: n

      CHARACTER(LEN=128) :: RESTFILE

      RESTFILE = 'REST_00000.FUNC'

      OPEN(20, FILE = RESTFILE, FORM = "FORMATTED")
      !> Write number of blocks
      WRITE(20,*) nblk
      DO iblk = 1, nblk
         WRITE(20,*) blk(iblk)%isize, blk(iblk)%jsize, blk(iblk)%ksize, 5
!         WRITE(20,*) blk(iblk)%isize, blk(iblk)%jsize, blk(iblk)%ksize
      END DO

      DO iblk = 1, nblk
!         WRITE(20,*) 1, 1.0, 100.0, 0.0
         WRITE(20,*) &
         (((blk(iblk)%flow%RHO(i,j,k), i=blk(iblk)%imin, blk(iblk)%imax), &
                                      j=blk(iblk)%jmin, blk(iblk)%jmax), &
                                      k=blk(iblk)%kmin, blk(iblk)%kmax), &
         (((blk(iblk)%flow%U(i,j,k), i=blk(iblk)%imin, blk(iblk)%imax), &
                                    j=blk(iblk)%jmin, blk(iblk)%jmax), &
                                    k=blk(iblk)%kmin, blk(iblk)%kmax), &
         (((blk(iblk)%flow%V(i,j,k), i=blk(iblk)%imin, blk(iblk)%imax), &
                                    j=blk(iblk)%jmin, blk(iblk)%jmax), &
                                    k=blk(iblk)%kmin, blk(iblk)%kmax), &
         (((blk(iblk)%flow%W(i,j,k), i=blk(iblk)%imin, blk(iblk)%imax), &
                                    j=blk(iblk)%jmin, blk(iblk)%jmax), &
                                    k=blk(iblk)%kmin, blk(iblk)%kmax), &
         (((blk(iblk)%flow%T(i,j,k), i=blk(iblk)%imin, blk(iblk)%imax), &
                                    j=blk(iblk)%jmin, blk(iblk)%jmax), &
                                    k=blk(iblk)%kmin, blk(iblk)%kmax)
!         ((((blk(iblk)%flow%Q(i,j,k,n), i=blk(iblk)%imin, blk(iblk)%imax), &
!                                       j=blk(iblk)%jmin, blk(iblk)%jmax), &
!                                       k=blk(iblk)%kmin, blk(iblk)%kmax), &
!                                       n=1,5)
      END DO
      CLOSE(20)

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE PrintErrorMessage(message, istop)
!-----------------------------------------------------------------------------!

      IMPLICIT NONE
      CHARACTER, INTENT(IN) :: message
      INTEGER, INTENT(INOUT) :: istop

      write(*,*) 'istop=', istop      
    
   END SUBROUTINE
END MODULE io_m
