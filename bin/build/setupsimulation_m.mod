GFORTRAN module version '6' created from /home/sayop/MyCFD/develcfd/src/main/SetupSimulation.F90 on Mon Jun  8 13:23:38 2015
MD5:a732574ebb09ae08fe2c3f8e56a02acb -- If you edit this, you'll get what you deserve.

(() () () () () () () () () () () () () () () () () () () () () () ()
() () () ())

()

()

()

()

()

(2 'initializeflowvars' 'setupsimulation_m' 'initializeflowvars' 1 ((
PROCEDURE UNKNOWN-INTENT MODULE-PROC DECL UNKNOWN 0 0 SUBROUTINE
ALWAYS_EXPLICIT) (UNKNOWN 0 0 0 UNKNOWN ()) 3 0 (4 5 6 7) () 0 () () ()
0 0)
8 'parameters_m' 'parameters_m' 'parameters_m' 1 ((MODULE UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN UNKNOWN 0 0) (UNKNOWN 0 0 0 UNKNOWN ()) 0 0 () () 0
() () () 0 0)
9 'setinitialconditions' 'setupsimulation_m' 'setinitialconditions' 1 (
(PROCEDURE UNKNOWN-INTENT MODULE-PROC DECL UNKNOWN 0 0 SUBROUTINE
IMPLICIT_PURE ALWAYS_EXPLICIT) (UNKNOWN 0 0 0 UNKNOWN ()) 10 0 (11 12 13)
() 0 () () () 0 0)
14 'setupsimulation_m' 'setupsimulation_m' 'setupsimulation_m' 1 ((
MODULE UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0) (UNKNOWN 0 0 0
UNKNOWN ()) 0 0 () () 0 () () () 0 0)
15 'wp' 'parameters_m' 'wp' 1 ((PARAMETER UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN IMPLICIT-SAVE 0 0) (INTEGER 4 0 0 INTEGER ()) 0 0 () (CONSTANT (
INTEGER 4 0 0 INTEGER ()) 0 '8') () 0 () () () 0 0)
4 'blk' '' 'blk' 3 ((VARIABLE INOUT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
ALLOCATABLE DIMENSION DUMMY) (DERIVED 16 0 0 DERIVED ()) 0 0 () (1 0
DEFERRED () ()) 0 () () () 0 0)
5 'flowvar' '' 'flowvar' 3 ((VARIABLE OUT UNKNOWN-PROC UNKNOWN UNKNOWN 0
0 ALLOCATABLE DIMENSION DUMMY) (DERIVED 17 0 0 DERIVED ()) 0 0 () (1 0
DEFERRED () ()) 0 () () () 0 0)
6 'nblk' '' 'nblk' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DUMMY) (INTEGER 4 0 0 INTEGER ()) 0 0 () () 0 () () () 0 0)
7 'ngc' '' 'ngc' 3 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 DUMMY)
(INTEGER 4 0 0 INTEGER ()) 0 0 () () 0 () () () 0 0)
17 'flowvars' 'flowvariables_m' 'flowvars' 3 ((DERIVED UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 ALLOC_COMP) (UNKNOWN 0 0 0 UNKNOWN ())
0 0 () () 0 ((18 'q' (REAL 8 0 0 REAL ()) (4 0 DEFERRED () () () () () ()
() ()) (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
ALLOCATABLE DIMENSION) UNKNOWN-ACCESS ()) (19 'u' (REAL 8 0 0 REAL ()) (
3 0 DEFERRED () () () () () ()) (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0 ALLOCATABLE DIMENSION) UNKNOWN-ACCESS ()) (20 'v' (
REAL 8 0 0 REAL ()) (3 0 DEFERRED () () () () () ()) (UNKNOWN-FL
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 ALLOCATABLE DIMENSION)
UNKNOWN-ACCESS ()) (21 'w' (REAL 8 0 0 REAL ()) (3 0 DEFERRED () () () ()
() ()) (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
ALLOCATABLE DIMENSION) UNKNOWN-ACCESS ()) (22 'p' (REAL 8 0 0 REAL ()) (
3 0 DEFERRED () () () () () ()) (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0 ALLOCATABLE DIMENSION) UNKNOWN-ACCESS ()) (23 'rho'
(REAL 8 0 0 REAL ()) (3 0 DEFERRED () () () () () ()) (UNKNOWN-FL
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 ALLOCATABLE DIMENSION)
UNKNOWN-ACCESS ()) (24 't' (REAL 8 0 0 REAL ()) (3 0 DEFERRED () () () ()
() ()) (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
ALLOCATABLE DIMENSION) UNKNOWN-ACCESS ())) PUBLIC (() () () ()) () 0 0
71245602)
16 'multiblock' 'multiblockvars_m' 'multiblock' 3 ((DERIVED
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 ALLOC_COMP) (UNKNOWN 0 0
0 UNKNOWN ()) 0 0 () () 0 ((25 'isize' (INTEGER 4 0 0 INTEGER ()) () (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0)
UNKNOWN-ACCESS ()) (26 'jsize' (INTEGER 4 0 0 INTEGER ()) () (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0)
UNKNOWN-ACCESS ()) (27 'ksize' (INTEGER 4 0 0 INTEGER ()) () (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0)
UNKNOWN-ACCESS ()) (28 'imin' (INTEGER 4 0 0 INTEGER ()) () (UNKNOWN-FL
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0) UNKNOWN-ACCESS ()) (29
'imax' (INTEGER 4 0 0 INTEGER ()) () (UNKNOWN-FL UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN UNKNOWN 0 0) UNKNOWN-ACCESS ()) (30 'jmin' (
INTEGER 4 0 0 INTEGER ()) () (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0) UNKNOWN-ACCESS ()) (31 'jmax' (INTEGER 4 0 0
INTEGER ()) () (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0
0) UNKNOWN-ACCESS ()) (32 'kmin' (INTEGER 4 0 0 INTEGER ()) () (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0)
UNKNOWN-ACCESS ()) (33 'kmax' (INTEGER 4 0 0 INTEGER ()) () (UNKNOWN-FL
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0) UNKNOWN-ACCESS ()) (34
'istart' (INTEGER 4 0 0 INTEGER ()) () (UNKNOWN-FL UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN UNKNOWN 0 0) UNKNOWN-ACCESS ()) (35 'iend' (
INTEGER 4 0 0 INTEGER ()) () (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0) UNKNOWN-ACCESS ()) (36 'jstart' (INTEGER 4 0 0
INTEGER ()) () (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0
0) UNKNOWN-ACCESS ()) (37 'jend' (INTEGER 4 0 0 INTEGER ()) () (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0)
UNKNOWN-ACCESS ()) (38 'kstart' (INTEGER 4 0 0 INTEGER ()) () (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0)
UNKNOWN-ACCESS ()) (39 'kend' (INTEGER 4 0 0 INTEGER ()) () (UNKNOWN-FL
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0) UNKNOWN-ACCESS ()) (40
'domainid' (INTEGER 4 0 0 INTEGER ()) () (UNKNOWN-FL UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN UNKNOWN 0 0) UNKNOWN-ACCESS ()) (41 'x' (REAL 8 0 0
REAL ()) (3 0 DEFERRED () () () () () ()) (UNKNOWN-FL UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 ALLOCATABLE DIMENSION) UNKNOWN-ACCESS ())
(42 'y' (REAL 8 0 0 REAL ()) (3 0 DEFERRED () () () () () ()) (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 ALLOCATABLE
DIMENSION) UNKNOWN-ACCESS ()) (43 'z' (REAL 8 0 0 REAL ()) (3 0 DEFERRED
() () () () () ()) (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN
UNKNOWN 0 0 ALLOCATABLE DIMENSION) UNKNOWN-ACCESS ()) (44 'bc_imin' (
INTEGER 4 0 0 INTEGER ()) () (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0) UNKNOWN-ACCESS ()) (45 'bc_imax' (INTEGER 4 0 0
INTEGER ()) () (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0
0) UNKNOWN-ACCESS ()) (46 'bc_jmin' (INTEGER 4 0 0 INTEGER ()) () (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0)
UNKNOWN-ACCESS ()) (47 'bc_jmax' (INTEGER 4 0 0 INTEGER ()) () (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0)
UNKNOWN-ACCESS ()) (48 'bc_kmin' (INTEGER 4 0 0 INTEGER ()) () (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0)
UNKNOWN-ACCESS ()) (49 'bc_kmax' (INTEGER 4 0 0 INTEGER ()) () (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0)
UNKNOWN-ACCESS ()) (50 'neighbor' (INTEGER 4 0 0 INTEGER ()) (3 0
EXPLICIT (CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '-1') (CONSTANT (
INTEGER 4 0 0 INTEGER ()) 0 '1') (CONSTANT (INTEGER 4 0 0 INTEGER ()) 0
'-1') (CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '1') (CONSTANT (INTEGER 4 0
0 INTEGER ()) 0 '-1') (CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '1')) (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 DIMENSION)
UNKNOWN-ACCESS ())) PUBLIC (() () () ()) () 0 0 19592773)
11 'blk' '' 'blk' 10 ((VARIABLE INOUT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
ALLOCATABLE DIMENSION DUMMY) (DERIVED 51 0 0 DERIVED ()) 0 0 () (1 0
DEFERRED () ()) 0 () () () 0 0)
12 'flowvar' '' 'flowvar' 10 ((VARIABLE INOUT UNKNOWN-PROC UNKNOWN
UNKNOWN 0 0 ALLOCATABLE DIMENSION DUMMY) (DERIVED 52 0 0 DERIVED ()) 0 0
() (1 0 DEFERRED () ()) 0 () () () 0 0)
13 'nblk' '' 'nblk' 10 ((VARIABLE IN UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
DUMMY) (INTEGER 4 0 0 INTEGER ()) 0 0 () () 0 () () () 0 0)
52 'flowvars' 'flowvariables_m' 'flowvars' 10 ((DERIVED UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 ALLOC_COMP) (UNKNOWN 0 0 0 UNKNOWN ())
0 0 () () 0 ((53 'q' (REAL 8 0 0 REAL ()) (4 0 DEFERRED () () () () () ()
() ()) (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
ALLOCATABLE DIMENSION) UNKNOWN-ACCESS ()) (54 'u' (REAL 8 0 0 REAL ()) (
3 0 DEFERRED () () () () () ()) (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0 ALLOCATABLE DIMENSION) UNKNOWN-ACCESS ()) (55 'v' (
REAL 8 0 0 REAL ()) (3 0 DEFERRED () () () () () ()) (UNKNOWN-FL
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 ALLOCATABLE DIMENSION)
UNKNOWN-ACCESS ()) (56 'w' (REAL 8 0 0 REAL ()) (3 0 DEFERRED () () () ()
() ()) (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
ALLOCATABLE DIMENSION) UNKNOWN-ACCESS ()) (57 'p' (REAL 8 0 0 REAL ()) (
3 0 DEFERRED () () () () () ()) (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0 ALLOCATABLE DIMENSION) UNKNOWN-ACCESS ()) (58 'rho'
(REAL 8 0 0 REAL ()) (3 0 DEFERRED () () () () () ()) (UNKNOWN-FL
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 ALLOCATABLE DIMENSION)
UNKNOWN-ACCESS ()) (59 't' (REAL 8 0 0 REAL ()) (3 0 DEFERRED () () () ()
() ()) (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0
ALLOCATABLE DIMENSION) UNKNOWN-ACCESS ())) PUBLIC (() () () ()) () 0 0
71245602)
51 'multiblock' 'multiblockvars_m' 'multiblock' 10 ((DERIVED
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 ALLOC_COMP) (UNKNOWN 0 0
0 UNKNOWN ()) 0 0 () () 0 ((60 'isize' (INTEGER 4 0 0 INTEGER ()) () (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0)
UNKNOWN-ACCESS ()) (61 'jsize' (INTEGER 4 0 0 INTEGER ()) () (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0)
UNKNOWN-ACCESS ()) (62 'ksize' (INTEGER 4 0 0 INTEGER ()) () (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0)
UNKNOWN-ACCESS ()) (63 'imin' (INTEGER 4 0 0 INTEGER ()) () (UNKNOWN-FL
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0) UNKNOWN-ACCESS ()) (64
'imax' (INTEGER 4 0 0 INTEGER ()) () (UNKNOWN-FL UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN UNKNOWN 0 0) UNKNOWN-ACCESS ()) (65 'jmin' (
INTEGER 4 0 0 INTEGER ()) () (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0) UNKNOWN-ACCESS ()) (66 'jmax' (INTEGER 4 0 0
INTEGER ()) () (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0
0) UNKNOWN-ACCESS ()) (67 'kmin' (INTEGER 4 0 0 INTEGER ()) () (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0)
UNKNOWN-ACCESS ()) (68 'kmax' (INTEGER 4 0 0 INTEGER ()) () (UNKNOWN-FL
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0) UNKNOWN-ACCESS ()) (69
'istart' (INTEGER 4 0 0 INTEGER ()) () (UNKNOWN-FL UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN UNKNOWN 0 0) UNKNOWN-ACCESS ()) (70 'iend' (
INTEGER 4 0 0 INTEGER ()) () (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0) UNKNOWN-ACCESS ()) (71 'jstart' (INTEGER 4 0 0
INTEGER ()) () (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0
0) UNKNOWN-ACCESS ()) (72 'jend' (INTEGER 4 0 0 INTEGER ()) () (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0)
UNKNOWN-ACCESS ()) (73 'kstart' (INTEGER 4 0 0 INTEGER ()) () (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0)
UNKNOWN-ACCESS ()) (74 'kend' (INTEGER 4 0 0 INTEGER ()) () (UNKNOWN-FL
UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0) UNKNOWN-ACCESS ()) (75
'domainid' (INTEGER 4 0 0 INTEGER ()) () (UNKNOWN-FL UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN UNKNOWN 0 0) UNKNOWN-ACCESS ()) (76 'x' (REAL 8 0 0
REAL ()) (3 0 DEFERRED () () () () () ()) (UNKNOWN-FL UNKNOWN-INTENT
UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 ALLOCATABLE DIMENSION) UNKNOWN-ACCESS ())
(77 'y' (REAL 8 0 0 REAL ()) (3 0 DEFERRED () () () () () ()) (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 ALLOCATABLE
DIMENSION) UNKNOWN-ACCESS ()) (78 'z' (REAL 8 0 0 REAL ()) (3 0 DEFERRED
() () () () () ()) (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN
UNKNOWN 0 0 ALLOCATABLE DIMENSION) UNKNOWN-ACCESS ()) (79 'bc_imin' (
INTEGER 4 0 0 INTEGER ()) () (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC
UNKNOWN UNKNOWN 0 0) UNKNOWN-ACCESS ()) (80 'bc_imax' (INTEGER 4 0 0
INTEGER ()) () (UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0
0) UNKNOWN-ACCESS ()) (81 'bc_jmin' (INTEGER 4 0 0 INTEGER ()) () (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0)
UNKNOWN-ACCESS ()) (82 'bc_jmax' (INTEGER 4 0 0 INTEGER ()) () (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0)
UNKNOWN-ACCESS ()) (83 'bc_kmin' (INTEGER 4 0 0 INTEGER ()) () (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0)
UNKNOWN-ACCESS ()) (84 'bc_kmax' (INTEGER 4 0 0 INTEGER ()) () (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0)
UNKNOWN-ACCESS ()) (85 'neighbor' (INTEGER 4 0 0 INTEGER ()) (3 0
EXPLICIT (CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '-1') (CONSTANT (
INTEGER 4 0 0 INTEGER ()) 0 '1') (CONSTANT (INTEGER 4 0 0 INTEGER ()) 0
'-1') (CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '1') (CONSTANT (INTEGER 4 0
0 INTEGER ()) 0 '-1') (CONSTANT (INTEGER 4 0 0 INTEGER ()) 0 '1')) (
UNKNOWN-FL UNKNOWN-INTENT UNKNOWN-PROC UNKNOWN UNKNOWN 0 0 DIMENSION)
UNKNOWN-ACCESS ())) PUBLIC (() () () ()) () 0 0 19592773)
)

('initializeflowvars' 0 2 'parameters_m' 0 8 'setinitialconditions' 0 9
'setupsimulation_m' 0 14 'wp' 0 15)