# Find MPI library
find_package(MPI REQUIRED COMPONENTS Fortran)

if(ENABLE_PNETCDF)
  add_definitions(-D__pnetcdf)
  include_directories(${PNETCDF_INCLUDES})
endif()

if(ENABLE_NETCDF)
  add_definitions(-D__netcdf)
  include_directories(${NETCDF_INCLUDES})
endif()

set(SOURCE_EXES  mo_mpi.f90
	        mo_domdcomp.f90
		mo_coupling.f90
		mo_nc.f90
		mo_nc4.f90
		dummys.f90
                )

if(SELF_CONTAINED)
  set(SOURCE_EXER  mo_mpi.f90
                   mo_domdcomp.f90
                   mo_coupling.f90
                   mo_nc.f90
                   mo_nc4.f90
                   dummyr.f90
                   )
endif()

#option(ENABLE_COUPLING "Enable coupling with YAC library" OFF)
if(ENABLE_COUPLING)
  add_definitions(-D__coupling)
  include_directories(${YAC_INCLUDE_DIRS})
endif()

# use rpath to avoid setting LD_LIBRARY_PATH at runtime
# set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

add_executable(dummys ${SOURCE_EXES})
if(ENABLE_COUPLING)
  if(ENABLE_PNETCDF)
    target_link_libraries (dummys ${PNETCDF_LIB} ${YAC_LDFLAGS})
  endif()
  if(ENABLE_NETCDF)
    target_link_libraries (dummys ${NETCDF_LIB} ${YAC_LDFLAGS})
  endif()
else()
  if(ENABLE_PNETCDF)
    target_link_libraries (dummys ${PNETCDF_LIB})
  endif()
  if(ENABLE_NETCDF)
    target_link_libraries (dummys ${NETCDF_LIB})
  endif()
endif()

if(SELF_CONTAINED)
  add_executable(dummyr ${SOURCE_EXER})
  if(ENABLE_COUPLING)
    if(ENABLE_PNETCDF)
      target_link_libraries (dummyr ${PNETCDF_LIB} ${YAC_LDFLAGS})
    endif()
    if(ENABLE_NETCDF)
      target_link_libraries (dummyr ${NETCDF_LIB} ${YAC_LDFLAGS})
    endif()
  else()
    if(ENABLE_PNETCDF)
      target_link_libraries (dummyr ${PNETCDF_LIB})
    endif()
    if(ENABLE_NETCDF)
      target_link_libraries (dummyr ${NETCDF_LIB})
    endif()
  endif()
endif()

# Install executables.
install (TARGETS
  dummys # executables
  RUNTIME DESTINATION ${PROJECT_SOURCE_DIR}/bin)

if(SELF_CONTAINED)
  install (TARGETS
    dummyr # executables
    RUNTIME DESTINATION ${PROJECT_SOURCE_DIR}/bin)
endif()
