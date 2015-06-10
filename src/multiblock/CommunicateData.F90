!> \file: CommunicateData.F90
!> \author: Sayop Kim

MODULE CommunicateData_m
   USE Parameters_m, ONLY: wp

CONTAINS

!-----------------------------------------------------------------------------!
   SUBROUTINE RecvDataFromNeighbor(blk, nblk, ngc)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: nblk, ngc
      INTEGER :: iblk, n, ii, jj, kk
      INTEGER :: iLoopStart, iLoopEnd, &
                 jLoopStart, jLoopEnd, &
                 kLoopStart, kLoopEnd
      INTEGER :: ibase, jbase, kbase
      INTEGER :: iRecv, jRecv, kRecv

      BlockLoop: DO iblk = 1, nblk

         iiLoop: DO ii = -1, 1
            jjLoop: DO jj = -1, 1
               kkLoop: DO kk = -1, 1

                  !> n: neighbor block index
                  n = blk(iblk)%neighbor(ii,jj,kk)

                  IF (n .NE. 0) THEN
                     !> Define ghost layer block looping range
                     CALL FindGhostLayerIndices(blk, iblk, n, ngc, ii, jj, kk, &
                                                iLoopStart, iLoopEnd, &
                                                jLoopStart, jLoopEnd, &
                                                kLoopStart, kLoopEnd, &
                                                ibase, jbase, kbase)
                     CALL CopyDataFromNeighbor(blk, iblk, n, ii, jj, kk, &
                                               iLoopStart, iLoopEnd, &
                                               jLoopStart, jLoopEnd, &
                                               kLoopStart, kLoopEnd, &
                                               ibase, jbase, kbase)
                  END IF

               END DO kkLoop
            END DO jjLoop
         END DO iiLoop

      END DO BlockLoop

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE CopyDataFromNeighbor(blk, m, n, ii, jj, kk, &
                                   iLoopStart, iLoopEnd, &
                                   jLoopStart, jLoopEnd, &
                                   kLoopStart, kLoopEnd, &
                                   ibase, jbase, kbase)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(INOUT) :: blk
      INTEGER, INTENT(IN) :: m, n, ii, jj, kk, iLoopStart, iLoopEnd, &
                                               jLoopStart, jLoopEnd, &
                                               kLoopStart, kLoopEnd
      INTEGER :: i, j, k, ibase, jbase, kbase, &
                 iRecv, jRecv, kRecv

      iRecv = ibase
      DO i = iLoopStart, iLoopEnd
         IF (ii .EQ. 0) iRecv = i
         jRecv = jbase
         DO j = jLoopStart, jLoopEnd
            IF (jj .EQ. 0) jRecv = j
            kRecv = kbase
            DO k = kLoopStart, kLoopEnd
               IF (kk .EQ. 0) kRecv = k
               blk(m)%flow%RHO(i,j,k) = blk(n)%flow%RHO(iRecv,jRecv,kRecv)
               blk(m)%flow%U(i,j,k)   = blk(n)%flow%U(iRecv,jRecv,kRecv)
               blk(m)%flow%V(i,j,k)   = blk(n)%flow%V(iRecv,jRecv,kRecv)
               blk(m)%flow%W(i,j,k)   = blk(n)%flow%W(iRecv,jRecv,kRecv)
               blk(m)%flow%T(i,j,k)   = blk(n)%flow%T(iRecv,jRecv,kRecv)
               blk(m)%flow%P(i,j,k)   = blk(n)%flow%P(iRecv,jRecv,kRecv)
               kRecv = kRecv + 1
            END DO
            jRecv = jRecv + 1
         END DO
         iRecv = iRecv + 1
      END DO

   END SUBROUTINE

!-----------------------------------------------------------------------------!
   SUBROUTINE FindGhostLayerIndices(blk, m, n, ngc, ii, jj, kk, &
                                    iLoopStart, iLoopEnd, &
                                    jLoopStart, jLoopEnd, &
                                    kLoopStart, kLoopEnd, &
                                    ibase, jbase, kbase)
!-----------------------------------------------------------------------------!
      USE MultiBlockVars_m, ONLY: MultiBlock

      IMPLICIT NONE
      TYPE(MultiBlock), DIMENSION(:), INTENT(IN) :: blk
      INTEGER, INTENT(OUT) :: iLoopStart, iLoopEnd, &
                              jLoopStart, jLoopEnd, &
                              kLoopStart, kLoopEnd, &
                              ibase, jbase, kbase
      INTEGER, INTENT(IN) :: m, n, ngc, ii, jj, kk

      SELECT CASE(ii)
      CASE(-1)
         iLoopStart = blk(m)%imin
         iLoopEnd = blk(m)%istart - 1
         IF (n .EQ. 0) THEN
            ibase = blk(m)%istart
         ELSE
            ibase = blk(n)%iend - ngc + 1
         END IF
      CASE(0)
         iLoopStart = blk(m)%istart
         iLoopEnd = blk(m)%iend
      CASE(1)
         iLoopStart = blk(m)%iend + 1
         iLoopEnd = blk(m)%imax
         IF (n .EQ. 0) THEN
            ibase = blk(m)%iend
         ELSE
            ibase = blk(n)%istart
         END IF
      END SELECT

      SELECT CASE(jj)
      CASE(-1)
         jLoopStart = blk(m)%jmin
         jLoopEnd = blk(m)%jstart - 1
         IF (n .EQ. 0) THEN
            jbase = blk(m)%jstart
         ELSE
            jbase = blk(n)%jend - ngc + 1
         END IF
      CASE(0)
         jLoopStart = blk(m)%jstart
         jLoopEnd = blk(m)%jend
      CASE(1)
         jLoopStart = blk(m)%jend + 1
         jLoopEnd = blk(m)%jmax
         IF (n .EQ. 0) THEN
            jbase = blk(m)%jend
         ELSE
            jbase = blk(n)%jstart
         END IF
      END SELECT

      SELECT CASE(kk)
      CASE(-1)
         kLoopStart = blk(m)%kmin
         kLoopEnd = blk(m)%kstart - 1
         IF (n .EQ. 0) THEN
            kbase = blk(m)%kstart
         ELSE
            kbase = blk(n)%kend - ngc + 1
         END IF
      CASE(0)
         kLoopStart = blk(m)%kstart
         kLoopEnd = blk(m)%kend
      CASE(1)
         kLoopStart = blk(m)%kend + 1
         kLoopEnd = blk(m)%kmax
         IF (n .EQ. 0) THEN
            kbase = blk(m)%kend
         ELSE
            kbase = blk(n)%kstart
         END IF
      END SELECT

   END SUBROUTINE

END MODULE
