# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 2.8

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:

# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list

# Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /home/sayop/LocalLibs/cmake-2.8.10.2-Linux-i386/bin/cmake

# The command to remove a file.
RM = /home/sayop/LocalLibs/cmake-2.8.10.2-Linux-i386/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The program to use to edit the cache.
CMAKE_EDIT_COMMAND = /home/sayop/LocalLibs/cmake-2.8.10.2-Linux-i386/bin/ccmake

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /home/sayop/MyCFD/develcfd/src

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /home/sayop/MyCFD/develcfd/bin256/build

# Include any dependencies generated for this target.
include CMakeFiles/cfd.x.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/cfd.x.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/cfd.x.dir/flags.make

CMakeFiles/cfd.x.dir/main/main.F90.o: CMakeFiles/cfd.x.dir/flags.make
CMakeFiles/cfd.x.dir/main/main.F90.o: /home/sayop/MyCFD/develcfd/src/main/main.F90
	$(CMAKE_COMMAND) -E cmake_progress_report /home/sayop/MyCFD/develcfd/bin256/build/CMakeFiles $(CMAKE_PROGRESS_1)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building Fortran object CMakeFiles/cfd.x.dir/main/main.F90.o"
	/usr/bin/gfortran  $(Fortran_DEFINES) $(Fortran_FLAGS) -c /home/sayop/MyCFD/develcfd/src/main/main.F90 -o CMakeFiles/cfd.x.dir/main/main.F90.o

CMakeFiles/cfd.x.dir/main/main.F90.o.requires:
.PHONY : CMakeFiles/cfd.x.dir/main/main.F90.o.requires

CMakeFiles/cfd.x.dir/main/main.F90.o.provides: CMakeFiles/cfd.x.dir/main/main.F90.o.requires
	$(MAKE) -f CMakeFiles/cfd.x.dir/build.make CMakeFiles/cfd.x.dir/main/main.F90.o.provides.build
.PHONY : CMakeFiles/cfd.x.dir/main/main.F90.o.provides

CMakeFiles/cfd.x.dir/main/main.F90.o.provides.build: CMakeFiles/cfd.x.dir/main/main.F90.o

CMakeFiles/cfd.x.dir/main/SetupSimulation.F90.o: CMakeFiles/cfd.x.dir/flags.make
CMakeFiles/cfd.x.dir/main/SetupSimulation.F90.o: /home/sayop/MyCFD/develcfd/src/main/SetupSimulation.F90
	$(CMAKE_COMMAND) -E cmake_progress_report /home/sayop/MyCFD/develcfd/bin256/build/CMakeFiles $(CMAKE_PROGRESS_2)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building Fortran object CMakeFiles/cfd.x.dir/main/SetupSimulation.F90.o"
	/usr/bin/gfortran  $(Fortran_DEFINES) $(Fortran_FLAGS) -c /home/sayop/MyCFD/develcfd/src/main/SetupSimulation.F90 -o CMakeFiles/cfd.x.dir/main/SetupSimulation.F90.o

CMakeFiles/cfd.x.dir/main/SetupSimulation.F90.o.requires:
.PHONY : CMakeFiles/cfd.x.dir/main/SetupSimulation.F90.o.requires

CMakeFiles/cfd.x.dir/main/SetupSimulation.F90.o.provides: CMakeFiles/cfd.x.dir/main/SetupSimulation.F90.o.requires
	$(MAKE) -f CMakeFiles/cfd.x.dir/build.make CMakeFiles/cfd.x.dir/main/SetupSimulation.F90.o.provides.build
.PHONY : CMakeFiles/cfd.x.dir/main/SetupSimulation.F90.o.provides

CMakeFiles/cfd.x.dir/main/SetupSimulation.F90.o.provides.build: CMakeFiles/cfd.x.dir/main/SetupSimulation.F90.o

CMakeFiles/cfd.x.dir/main/Parameters.F90.o: CMakeFiles/cfd.x.dir/flags.make
CMakeFiles/cfd.x.dir/main/Parameters.F90.o: /home/sayop/MyCFD/develcfd/src/main/Parameters.F90
	$(CMAKE_COMMAND) -E cmake_progress_report /home/sayop/MyCFD/develcfd/bin256/build/CMakeFiles $(CMAKE_PROGRESS_3)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building Fortran object CMakeFiles/cfd.x.dir/main/Parameters.F90.o"
	/usr/bin/gfortran  $(Fortran_DEFINES) $(Fortran_FLAGS) -c /home/sayop/MyCFD/develcfd/src/main/Parameters.F90 -o CMakeFiles/cfd.x.dir/main/Parameters.F90.o

CMakeFiles/cfd.x.dir/main/Parameters.F90.o.requires:
.PHONY : CMakeFiles/cfd.x.dir/main/Parameters.F90.o.requires

CMakeFiles/cfd.x.dir/main/Parameters.F90.o.provides: CMakeFiles/cfd.x.dir/main/Parameters.F90.o.requires
	$(MAKE) -f CMakeFiles/cfd.x.dir/build.make CMakeFiles/cfd.x.dir/main/Parameters.F90.o.provides.build
.PHONY : CMakeFiles/cfd.x.dir/main/Parameters.F90.o.provides

CMakeFiles/cfd.x.dir/main/Parameters.F90.o.provides.build: CMakeFiles/cfd.x.dir/main/Parameters.F90.o

CMakeFiles/cfd.x.dir/io/input.f90.o: CMakeFiles/cfd.x.dir/flags.make
CMakeFiles/cfd.x.dir/io/input.f90.o: io/input.f90
	$(CMAKE_COMMAND) -E cmake_progress_report /home/sayop/MyCFD/develcfd/bin256/build/CMakeFiles $(CMAKE_PROGRESS_4)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building Fortran object CMakeFiles/cfd.x.dir/io/input.f90.o"
	/usr/bin/gfortran  $(Fortran_DEFINES) $(Fortran_FLAGS) -c /home/sayop/MyCFD/develcfd/bin256/build/io/input.f90 -o CMakeFiles/cfd.x.dir/io/input.f90.o

CMakeFiles/cfd.x.dir/io/input.f90.o.requires:
.PHONY : CMakeFiles/cfd.x.dir/io/input.f90.o.requires

CMakeFiles/cfd.x.dir/io/input.f90.o.provides: CMakeFiles/cfd.x.dir/io/input.f90.o.requires
	$(MAKE) -f CMakeFiles/cfd.x.dir/build.make CMakeFiles/cfd.x.dir/io/input.f90.o.provides.build
.PHONY : CMakeFiles/cfd.x.dir/io/input.f90.o.provides

CMakeFiles/cfd.x.dir/io/input.f90.o.provides.build: CMakeFiles/cfd.x.dir/io/input.f90.o

CMakeFiles/cfd.x.dir/io/io.F90.o: CMakeFiles/cfd.x.dir/flags.make
CMakeFiles/cfd.x.dir/io/io.F90.o: /home/sayop/MyCFD/develcfd/src/io/io.F90
	$(CMAKE_COMMAND) -E cmake_progress_report /home/sayop/MyCFD/develcfd/bin256/build/CMakeFiles $(CMAKE_PROGRESS_5)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building Fortran object CMakeFiles/cfd.x.dir/io/io.F90.o"
	/usr/bin/gfortran  $(Fortran_DEFINES) $(Fortran_FLAGS) -c /home/sayop/MyCFD/develcfd/src/io/io.F90 -o CMakeFiles/cfd.x.dir/io/io.F90.o

CMakeFiles/cfd.x.dir/io/io.F90.o.requires:
.PHONY : CMakeFiles/cfd.x.dir/io/io.F90.o.requires

CMakeFiles/cfd.x.dir/io/io.F90.o.provides: CMakeFiles/cfd.x.dir/io/io.F90.o.requires
	$(MAKE) -f CMakeFiles/cfd.x.dir/build.make CMakeFiles/cfd.x.dir/io/io.F90.o.provides.build
.PHONY : CMakeFiles/cfd.x.dir/io/io.F90.o.provides

CMakeFiles/cfd.x.dir/io/io.F90.o.provides.build: CMakeFiles/cfd.x.dir/io/io.F90.o

CMakeFiles/cfd.x.dir/variables/MultiBlockVars.F90.o: CMakeFiles/cfd.x.dir/flags.make
CMakeFiles/cfd.x.dir/variables/MultiBlockVars.F90.o: /home/sayop/MyCFD/develcfd/src/variables/MultiBlockVars.F90
	$(CMAKE_COMMAND) -E cmake_progress_report /home/sayop/MyCFD/develcfd/bin256/build/CMakeFiles $(CMAKE_PROGRESS_6)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building Fortran object CMakeFiles/cfd.x.dir/variables/MultiBlockVars.F90.o"
	/usr/bin/gfortran  $(Fortran_DEFINES) $(Fortran_FLAGS) -c /home/sayop/MyCFD/develcfd/src/variables/MultiBlockVars.F90 -o CMakeFiles/cfd.x.dir/variables/MultiBlockVars.F90.o

CMakeFiles/cfd.x.dir/variables/MultiBlockVars.F90.o.requires:
.PHONY : CMakeFiles/cfd.x.dir/variables/MultiBlockVars.F90.o.requires

CMakeFiles/cfd.x.dir/variables/MultiBlockVars.F90.o.provides: CMakeFiles/cfd.x.dir/variables/MultiBlockVars.F90.o.requires
	$(MAKE) -f CMakeFiles/cfd.x.dir/build.make CMakeFiles/cfd.x.dir/variables/MultiBlockVars.F90.o.provides.build
.PHONY : CMakeFiles/cfd.x.dir/variables/MultiBlockVars.F90.o.provides

CMakeFiles/cfd.x.dir/variables/MultiBlockVars.F90.o.provides.build: CMakeFiles/cfd.x.dir/variables/MultiBlockVars.F90.o

CMakeFiles/cfd.x.dir/variables/FlowVariables.F90.o: CMakeFiles/cfd.x.dir/flags.make
CMakeFiles/cfd.x.dir/variables/FlowVariables.F90.o: /home/sayop/MyCFD/develcfd/src/variables/FlowVariables.F90
	$(CMAKE_COMMAND) -E cmake_progress_report /home/sayop/MyCFD/develcfd/bin256/build/CMakeFiles $(CMAKE_PROGRESS_7)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building Fortran object CMakeFiles/cfd.x.dir/variables/FlowVariables.F90.o"
	/usr/bin/gfortran  $(Fortran_DEFINES) $(Fortran_FLAGS) -c /home/sayop/MyCFD/develcfd/src/variables/FlowVariables.F90 -o CMakeFiles/cfd.x.dir/variables/FlowVariables.F90.o

CMakeFiles/cfd.x.dir/variables/FlowVariables.F90.o.requires:
.PHONY : CMakeFiles/cfd.x.dir/variables/FlowVariables.F90.o.requires

CMakeFiles/cfd.x.dir/variables/FlowVariables.F90.o.provides: CMakeFiles/cfd.x.dir/variables/FlowVariables.F90.o.requires
	$(MAKE) -f CMakeFiles/cfd.x.dir/build.make CMakeFiles/cfd.x.dir/variables/FlowVariables.F90.o.provides.build
.PHONY : CMakeFiles/cfd.x.dir/variables/FlowVariables.F90.o.provides

CMakeFiles/cfd.x.dir/variables/FlowVariables.F90.o.provides.build: CMakeFiles/cfd.x.dir/variables/FlowVariables.F90.o

CMakeFiles/cfd.x.dir/variables/AllocateVariables.F90.o: CMakeFiles/cfd.x.dir/flags.make
CMakeFiles/cfd.x.dir/variables/AllocateVariables.F90.o: /home/sayop/MyCFD/develcfd/src/variables/AllocateVariables.F90
	$(CMAKE_COMMAND) -E cmake_progress_report /home/sayop/MyCFD/develcfd/bin256/build/CMakeFiles $(CMAKE_PROGRESS_8)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building Fortran object CMakeFiles/cfd.x.dir/variables/AllocateVariables.F90.o"
	/usr/bin/gfortran  $(Fortran_DEFINES) $(Fortran_FLAGS) -c /home/sayop/MyCFD/develcfd/src/variables/AllocateVariables.F90 -o CMakeFiles/cfd.x.dir/variables/AllocateVariables.F90.o

CMakeFiles/cfd.x.dir/variables/AllocateVariables.F90.o.requires:
.PHONY : CMakeFiles/cfd.x.dir/variables/AllocateVariables.F90.o.requires

CMakeFiles/cfd.x.dir/variables/AllocateVariables.F90.o.provides: CMakeFiles/cfd.x.dir/variables/AllocateVariables.F90.o.requires
	$(MAKE) -f CMakeFiles/cfd.x.dir/build.make CMakeFiles/cfd.x.dir/variables/AllocateVariables.F90.o.provides.build
.PHONY : CMakeFiles/cfd.x.dir/variables/AllocateVariables.F90.o.provides

CMakeFiles/cfd.x.dir/variables/AllocateVariables.F90.o.provides.build: CMakeFiles/cfd.x.dir/variables/AllocateVariables.F90.o

CMakeFiles/cfd.x.dir/multiblock/InitMultiBlock.F90.o: CMakeFiles/cfd.x.dir/flags.make
CMakeFiles/cfd.x.dir/multiblock/InitMultiBlock.F90.o: /home/sayop/MyCFD/develcfd/src/multiblock/InitMultiBlock.F90
	$(CMAKE_COMMAND) -E cmake_progress_report /home/sayop/MyCFD/develcfd/bin256/build/CMakeFiles $(CMAKE_PROGRESS_9)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building Fortran object CMakeFiles/cfd.x.dir/multiblock/InitMultiBlock.F90.o"
	/usr/bin/gfortran  $(Fortran_DEFINES) $(Fortran_FLAGS) -c /home/sayop/MyCFD/develcfd/src/multiblock/InitMultiBlock.F90 -o CMakeFiles/cfd.x.dir/multiblock/InitMultiBlock.F90.o

CMakeFiles/cfd.x.dir/multiblock/InitMultiBlock.F90.o.requires:
.PHONY : CMakeFiles/cfd.x.dir/multiblock/InitMultiBlock.F90.o.requires

CMakeFiles/cfd.x.dir/multiblock/InitMultiBlock.F90.o.provides: CMakeFiles/cfd.x.dir/multiblock/InitMultiBlock.F90.o.requires
	$(MAKE) -f CMakeFiles/cfd.x.dir/build.make CMakeFiles/cfd.x.dir/multiblock/InitMultiBlock.F90.o.provides.build
.PHONY : CMakeFiles/cfd.x.dir/multiblock/InitMultiBlock.F90.o.provides

CMakeFiles/cfd.x.dir/multiblock/InitMultiBlock.F90.o.provides.build: CMakeFiles/cfd.x.dir/multiblock/InitMultiBlock.F90.o

CMakeFiles/cfd.x.dir/multiblock/CommunicateData.F90.o: CMakeFiles/cfd.x.dir/flags.make
CMakeFiles/cfd.x.dir/multiblock/CommunicateData.F90.o: /home/sayop/MyCFD/develcfd/src/multiblock/CommunicateData.F90
	$(CMAKE_COMMAND) -E cmake_progress_report /home/sayop/MyCFD/develcfd/bin256/build/CMakeFiles $(CMAKE_PROGRESS_10)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building Fortran object CMakeFiles/cfd.x.dir/multiblock/CommunicateData.F90.o"
	/usr/bin/gfortran  $(Fortran_DEFINES) $(Fortran_FLAGS) -c /home/sayop/MyCFD/develcfd/src/multiblock/CommunicateData.F90 -o CMakeFiles/cfd.x.dir/multiblock/CommunicateData.F90.o

CMakeFiles/cfd.x.dir/multiblock/CommunicateData.F90.o.requires:
.PHONY : CMakeFiles/cfd.x.dir/multiblock/CommunicateData.F90.o.requires

CMakeFiles/cfd.x.dir/multiblock/CommunicateData.F90.o.provides: CMakeFiles/cfd.x.dir/multiblock/CommunicateData.F90.o.requires
	$(MAKE) -f CMakeFiles/cfd.x.dir/build.make CMakeFiles/cfd.x.dir/multiblock/CommunicateData.F90.o.provides.build
.PHONY : CMakeFiles/cfd.x.dir/multiblock/CommunicateData.F90.o.provides

CMakeFiles/cfd.x.dir/multiblock/CommunicateData.F90.o.provides.build: CMakeFiles/cfd.x.dir/multiblock/CommunicateData.F90.o

# Object files for target cfd.x
cfd_x_OBJECTS = \
"CMakeFiles/cfd.x.dir/main/main.F90.o" \
"CMakeFiles/cfd.x.dir/main/SetupSimulation.F90.o" \
"CMakeFiles/cfd.x.dir/main/Parameters.F90.o" \
"CMakeFiles/cfd.x.dir/io/input.f90.o" \
"CMakeFiles/cfd.x.dir/io/io.F90.o" \
"CMakeFiles/cfd.x.dir/variables/MultiBlockVars.F90.o" \
"CMakeFiles/cfd.x.dir/variables/FlowVariables.F90.o" \
"CMakeFiles/cfd.x.dir/variables/AllocateVariables.F90.o" \
"CMakeFiles/cfd.x.dir/multiblock/InitMultiBlock.F90.o" \
"CMakeFiles/cfd.x.dir/multiblock/CommunicateData.F90.o"

# External object files for target cfd.x
cfd_x_EXTERNAL_OBJECTS =

cfd.x: CMakeFiles/cfd.x.dir/main/main.F90.o
cfd.x: CMakeFiles/cfd.x.dir/main/SetupSimulation.F90.o
cfd.x: CMakeFiles/cfd.x.dir/main/Parameters.F90.o
cfd.x: CMakeFiles/cfd.x.dir/io/input.f90.o
cfd.x: CMakeFiles/cfd.x.dir/io/io.F90.o
cfd.x: CMakeFiles/cfd.x.dir/variables/MultiBlockVars.F90.o
cfd.x: CMakeFiles/cfd.x.dir/variables/FlowVariables.F90.o
cfd.x: CMakeFiles/cfd.x.dir/variables/AllocateVariables.F90.o
cfd.x: CMakeFiles/cfd.x.dir/multiblock/InitMultiBlock.F90.o
cfd.x: CMakeFiles/cfd.x.dir/multiblock/CommunicateData.F90.o
cfd.x: CMakeFiles/cfd.x.dir/build.make
cfd.x: CMakeFiles/cfd.x.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --red --bold "Linking Fortran executable cfd.x"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/cfd.x.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/cfd.x.dir/build: cfd.x
.PHONY : CMakeFiles/cfd.x.dir/build

CMakeFiles/cfd.x.dir/requires: CMakeFiles/cfd.x.dir/main/main.F90.o.requires
CMakeFiles/cfd.x.dir/requires: CMakeFiles/cfd.x.dir/main/SetupSimulation.F90.o.requires
CMakeFiles/cfd.x.dir/requires: CMakeFiles/cfd.x.dir/main/Parameters.F90.o.requires
CMakeFiles/cfd.x.dir/requires: CMakeFiles/cfd.x.dir/io/input.f90.o.requires
CMakeFiles/cfd.x.dir/requires: CMakeFiles/cfd.x.dir/io/io.F90.o.requires
CMakeFiles/cfd.x.dir/requires: CMakeFiles/cfd.x.dir/variables/MultiBlockVars.F90.o.requires
CMakeFiles/cfd.x.dir/requires: CMakeFiles/cfd.x.dir/variables/FlowVariables.F90.o.requires
CMakeFiles/cfd.x.dir/requires: CMakeFiles/cfd.x.dir/variables/AllocateVariables.F90.o.requires
CMakeFiles/cfd.x.dir/requires: CMakeFiles/cfd.x.dir/multiblock/InitMultiBlock.F90.o.requires
CMakeFiles/cfd.x.dir/requires: CMakeFiles/cfd.x.dir/multiblock/CommunicateData.F90.o.requires
.PHONY : CMakeFiles/cfd.x.dir/requires

CMakeFiles/cfd.x.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/cfd.x.dir/cmake_clean.cmake
.PHONY : CMakeFiles/cfd.x.dir/clean

CMakeFiles/cfd.x.dir/depend:
	cd /home/sayop/MyCFD/develcfd/bin256/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/sayop/MyCFD/develcfd/src /home/sayop/MyCFD/develcfd/src /home/sayop/MyCFD/develcfd/bin256/build /home/sayop/MyCFD/develcfd/bin256/build /home/sayop/MyCFD/develcfd/bin256/build/CMakeFiles/cfd.x.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/cfd.x.dir/depend

