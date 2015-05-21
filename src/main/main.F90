!> \file: main.F90
!> \author: Sayop Kim

PROGRAM main
   USE Parameters_m, ONLY: wp
   USE MultiDomainVars_m
   USE io_m
   USE InitMultiDomain_m

   IMPLICIT NONE
   TYPE(MultiDomain), DIMENSION(:), ALLOCATABLE, TARGET :: dom

   !WRITE(*,*) 'SAYOP'
   CALL ReadInputFiles()
   !> Read grid file and allocate multi-domain variables
   CALL InitializeMultiDomain(dom)
   !CALL WriteInputFiles()

END PROGRAM main
