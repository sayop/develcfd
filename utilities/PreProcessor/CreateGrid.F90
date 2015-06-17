!> \file: CreateGrid.F90
!> \author: Sayop Kim
!> \brief: Provides routines to read structured grid in plot3d file format.

MODULE CreateGrid_m
   USE Parameters_m, ONLY: wp

CONTAINS

!-----------------------------------------------------------------------------!
   SUBROUTINE Create1DomainGrid(ndomain, nblk, dom, blk, ngc)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock, MultiDomain, &
                                  AllocateMultiBlockXYZ
      USE xml_data_input
      USE PreSetup_m, ONLY: WriteGRID, WriteBCinfo

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(OUT) :: blk
      TYPE(MultiDomain), DIMENSION(:), INTENT(OUT) :: dom
      INTEGER, INTENT(IN) :: ndomain, ngc
      INTEGER, INTENT(INOUT) :: nblk
      INTEGER :: i, j, k, iblk, n
      INTEGER, DIMENSION(3) :: ndiv
      INTEGER :: iEvenSize, jEvenSize, kEvenSize
      INTEGER :: isize, jsize, ksize
      REAL(KIND=wp) :: dx, dy, dz
      CHARACTER(LEN=128) :: GRIDFILE

      WRITE(*,*) ''
      WRITE(*,*) '# Creating a single domain structured grid in plot3d format.'

      IF (ndomain .NE. 1) THEN
         WRITE(*,*) '------------------------------------------------------'
         WRITE(*,*) 'WARNING: Please check "ndomain" in input file!'
         WRITE(*,*) 'The number of domain should be set to 1 for this case.'
         WRITE(*,*) '------------------------------------------------------'
         STOP
      END IF
      
      dom(1)%istart = 1
      dom(1)%jstart = 1
      dom(1)%kstart = 1
      isize = input_data%Geometry%isize
      jsize = input_data%Geometry%jsize
      ksize = input_data%Geometry%ksize
      dom(1)%iend = isize
      dom(1)%jend = jsize
      dom(1)%kend = ksize
      ndiv = input_data%Geometry%ndivide

      !> Read BC info from inputfile
      dom(1)%bc_imin = input_data%BoundaryCondition%imin
      dom(1)%bc_imax = input_data%BoundaryCondition%imax
      dom(1)%bc_jmin = input_data%BoundaryCondition%jmin
      dom(1)%bc_jmax = input_data%BoundaryCondition%jmax
      dom(1)%bc_kmin = input_data%BoundaryCondition%kmin
      dom(1)%bc_kmax = input_data%BoundaryCondition%kmax

      n = 1
      DO i = 1, 3
         n = n * ndiv(i)
      END DO
      IF (n .NE. nblk) THEN
         WRITE(*,*) '--------------------------------------------------------'
         WRITE(*,*) 'WARNING: Please check "nblk" or "ndivide" in input file!'
         WRITE(*,*) '--------------------------------------------------------'
         STOP
      END IF

      !> Allocate memory for storing blockID info into each domain
      dom(1)%nblocks = nblk
      ALLOCATE(dom(1)%blockID(dom(1)%nblocks))

      !> Initialize node point coordinates
      ALLOCATE(dom(1)%x(isize,jsize,ksize))
      ALLOCATE(dom(1)%y(isize,jsize,ksize))
      ALLOCATE(dom(1)%z(isize,jsize,ksize))

      dx = input_data%Geometry%xlen / (isize - 1)
      dy = input_data%Geometry%ylen / (jsize - 1)
      dz = input_data%Geometry%zlen / (ksize - 1)

      DO i = 1, isize
         DO j = 1, jsize
            DO k = 1, ksize
               dom(1)%x(i,j,k) = StretchingFn(input_data%Geometry%xstart, &
                                              input_data%Geometry%xlen, &
                                              isize, i, &
                                              input_data%GridStretch%xpower, &
                                              input_data%GridStretch%xstrength)
               dom(1)%y(i,j,k) = StretchingFn(input_data%Geometry%ystart, &
                                              input_data%Geometry%ylen, &
                                              jsize, j, &
                                              input_data%GridStretch%ypower, &
                                              input_data%GridStretch%ystrength)
               dom(1)%z(i,j,k) = StretchingFn(input_data%Geometry%zstart, &
                                              input_data%Geometry%zlen, &
                                              ksize, k, &
                                              input_data%GridStretch%zpower, &
                                              input_data%GridStretch%zstrength)
            END DO
         END DO
      END DO

      IF ((isize - 2 .LT. (ndiv(1) - 1)) .OR. &
          (jsize - 2 .LT. (ndiv(2) - 1)) .OR. &
          (ksize - 2 .LT. (ndiv(3) - 1))) THEN
          WRITE(*,*) "--------------------------------------------------------"
          WRITE(*,*) "Can't divide the domain with this 'ndivide' setup!"
          WRITE(*,*) "Please re-adjust 'ndivide' values regarding domain size."
          WRITE(*,*) "--------------------------------------------------------"
          STOP
      END IF

      !> Determine the size of evenly divided blocks
      iEvenSize = NINT(REAL(isize + ndiv(1) - 1) / REAL(ndiv(1)))
      jEvenSize = NINT(REAL(jsize + ndiv(2) - 1) / REAL(ndiv(2)))
      kEvenSize = NINT(REAL(ksize + ndiv(3) - 1) / REAL(ndiv(3)))


      n = 1
      DO i = 1, ndiv(1)
         DO j = 1, ndiv(2)
            DO k = 1, ndiv(3)
               !> Divide corresponding domain in i-direction 
               blk(n)%istart = 1 + (i - 1) * (iEvenSize - 1)
               blk(n)%imin = blk(n)%istart
               IF (i .NE. ndiv(1)) THEN
                  blk(n)%iend = blk(n)%istart + iEvenSize - 1
               ELSE
                  blk(n)%iend = dom(1)%iend
               END IF
               blk(n)%imax = blk(n)%iend
               blk(n)%isize = blk(n)%imax - blk(n)%imin + 1
               !> Divide corresponding domain in j-direction
               blk(n)%jstart = 1 + (j - 1) * (jEvenSize - 1)
               blk(n)%jmin = blk(n)%jstart
               IF (j .NE. ndiv(2)) THEN
                  blk(n)%jend = blk(n)%jstart + jEvenSize - 1
               ELSE
                  blk(n)%jend = dom(1)%jend
               END IF
               blk(n)%jmax = blk(n)%jend
               blk(n)%jsize = blk(n)%jmax - blk(n)%jmin + 1
               !> Divide corresponding domain in k-direction
               blk(n)%kstart = 1 + (k - 1) * (kEvenSize - 1)
               blk(n)%kmin = blk(n)%kstart
               IF (k .NE. ndiv(3)) THEN
                  blk(n)%kend = blk(n)%kstart + kEvenSize - 1
               ELSE
                  blk(n)%kend = dom(1)%kend
               END IF
               blk(n)%kmax = blk(n)%kend
               blk(n)%ksize = blk(n)%kmax - blk(n)%kmin + 1

               !> Update blockID in the corresponding domain
               dom(1)%blockID(n) = n
               n = n + 1
            END DO
         END DO
      END DO

     
      !> Assign node point coordinates to each blocks
      BlockLoop: DO iblk = 1, nblk
         CALL AllocateMultiBlockXYZ(blk(iblk), blk(iblk)%imin-ngc, &
                                               blk(iblk)%imax+ngc, &
                                               blk(iblk)%jmin-ngc, &
                                               blk(iblk)%jmax+ngc, &
                                               blk(iblk)%kmin-ngc, &
                                               blk(iblk)%kmax+ngc)

         blk(iblk)%domainID = 1

         DO i = blk(iblk)%imin, blk(iblk)%imax
            DO j = blk(iblk)%jmin, blk(iblk)%jmax
               DO k = blk(iblk)%kmin, blk(iblk)%kmax
                  blk(iblk)%x(i,j,k) = dom(1)%x(i,j,k)
                  blk(iblk)%y(i,j,k) = dom(1)%y(i,j,k)
                  blk(iblk)%z(i,j,k) = dom(1)%z(i,j,k)
               END DO
            END DO
         END DO
      END DO BlockLoop


      !> Write a PLOT3D structured grid: grid.dat
      GRIDFILE = 'grid.dat'
      CALL WriteGRID(nblk, blk, GRIDFILE) 

      !> Write bcinfo.dat: ndomain = 1
      CALL WriteBCinfo(ndomain, dom, nblk, blk)

   END SUBROUTINE

   FUNCTION StretchingFn(lineStart, lineLength, nsize, indx, power, strength) RESULT(outcome)
   !> This function provides xi(i) function to make stretched line.
      REAL(KIND=wp), INTENT(IN) :: lineStart, lineLength, power, strength
      INTEGER, INTENT(IN) :: nsize, indx
      INTEGER :: ncell
      REAL(KIND=wp) :: xi

      ncell = nsize - 1
      xi = REAL(indx - 1) / REAL(ncell)

      xi = xi ** (abs(power))

      !> one-sided boundary layer stretching
      outcome = lineLength * xi

      !> internal layer stretching
      outcome = outcome + strength * (lineStart + 0.5_wp * lineLength - lineLength * xi) * &
                                     (1.0_wp - xi) * xi
      outcome = lineStart + outcome
   END FUNCTION StretchingFn

END MODULE


