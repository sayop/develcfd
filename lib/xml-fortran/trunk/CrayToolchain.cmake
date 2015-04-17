# the name of the target operating system
SET(CMAKE_SYSTEM_NAME Catamount)

# set the compiler
set(CMAKE_C_COMPILER cc -target=native)
set(CMAKE_CXX_COMPILER CC -target=native)
set(CMAKE_Fortran_COMPILER ftn -target=native)

# set the search path for the environment coming with the compiler
# and a directory where you can install your own compiled software
#set(CMAKE_FIND_ROOT_PATH
#    /opt/xt-pe/default
#    /opt/xt-mpt/default/mpich2-64/GP2
#    /home/alex/cray-install
#  )

# adjust the default behaviour of the FIND_XXX() commands:
# search headers and libraries in the target environment, search
# programs in the host environment
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
