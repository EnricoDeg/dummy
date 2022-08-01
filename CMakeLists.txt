cmake_minimum_required(VERSION 3.0)

project(dummy Fortran)

if(ENABLE_PNETCDF)
  include ("CMake/FindPNetCDF.cmake")
endif()

if(ENABLE_NETCDF)
  include ("CMake/FindNetCDF.cmake")
endif()

option(ENABLE_COUPLING "Enable coupling with YAC library" OFF)
if(ENABLE_COUPLING)
  include ("CMake/FindYAC.cmake")
endif()

add_subdirectory(src)
