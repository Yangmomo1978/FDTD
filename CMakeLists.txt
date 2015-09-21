cmake_minimum_required (VERSION 2.6)

project(FDTD Fortran)

set(MY_SOURCES main.f90 src/FDTD_Constants.f90 src/FDTD.f90
               src/Grid.f90 src/PrintField.f90 src/TFSF.f90
               src/CPML.f90)

set(BIN ${CMAKE_SOURCE_DIR}/bin)

set(CMAKE_Fortran_MODULE_DIRECTORY ${BIN})

set(CMAKE_EXE_LINKER_FLAGS)# "-static")
add_executable(FDTD ${MY_SOURCES})

