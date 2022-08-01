#  Find NetCDF
#  Find NetCDF includes and library
#
#  NETCDF_INCLUDES    - where to find pnetcdf.h
#  NETCDF_LIBRARIES   - List of libraries to link with when using NetCDF
#  NETCDF_FOUND       - True if NetCDF was found

if (NETCDF_INCLUDES)
  # Already in cache, be silent
  set (NETCDF_FIND_QUIETLY TRUE)
endif (NETCDF_INCLUDES)

find_path (NETCDF_INCLUDES netcdf.inc
	HINTS "${NETCDF_ROOT}/include" "$ENV{NETCDF_ROOT}/include")

string(REGEX REPLACE "/include/?$" "/lib"
	NETCDF_LIB_HINT ${NETCDF_INCLUDES})

find_library (NETCDF_LIB
  NAMES netcdff
  HINTS ${NETCDF_LIB_HINT})

if ((NOT NETCDF_LIB) OR (NOT NETCDF_INCLUDES))
  message(STATUS "Trying to find NetCDF using LD_LIBRARY_PATH (we're desperate)...")

  file(TO_CMAKE_PATH "$ENV{LD_LIBRARY_PATH}" LD_LIBRARY_PATH)

  find_library(NETCDF_LIB
    NAMES netcdff
    HINTS ${LD_LIBRARY_PATH})

  if (NETCDF_LIB)
	  get_filename_component(NETCDF_LIB_DIR ${NETCDF_LIB} PATH)
    string(REGEX REPLACE "/lib/?$" "/include"
	    NETCDF_H_HINT ${NETCDF_LIB_DIR})

    find_path (NETCDF_INCLUDES netcdf.inc
	    HINTS ${NETCDF_H_HINT}
      DOC "Path to netcdf.inc")
  endif()
endif()

if (NETCDF_LIB AND NETCDF_INCLUDES)
	set (NETCDF_LIBRARIES "${NETCDF_LIB}")
endif()

# handle the QUIETLY and REQUIRED arguments and set YAXT_FOUND to TRUE if
# all listed variables are TRUE
include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (NETCDF DEFAULT_MSG NETCDF_LIBRARIES NETCDF_INCLUDES)

mark_as_advanced (NETCDF_LIB NETCDF_INCLUDES)
