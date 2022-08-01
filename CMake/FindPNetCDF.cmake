#  Find PNetCDF
#  Find PNetCDF includes and library
#
#  PNETCDF_INCLUDES    - where to find pnetcdf.h
#  PNETCDF_LIBRARIES   - List of libraries to link with when using PNetCDF
#  PNETCDF_FOUND       - True if PNetCDF was found

if (PNETCDF_INCLUDES)
  # Already in cache, be silent
  set (PNETCDF_FIND_QUIETLY TRUE)
endif (PNETCDF_INCLUDES)

find_path (PNETCDF_INCLUDES pnetcdf.h
	HINTS "${PNETCDF_ROOT}/include" "$ENV{PNETCDF_ROOT}/include")

string(REGEX REPLACE "/include/?$" "/lib"
	PNETCDF_LIB_HINT ${PNETCDF_INCLUDES})

find_library (PNETCDF_LIB
  NAMES pnetcdf
  HINTS ${PNETCDF_LIB_HINT})

if ((NOT PNETCDF_LIB) OR (NOT PNETCDF_INCLUDES))
  message(STATUS "Trying to find PNetCDF using LD_LIBRARY_PATH (we're desperate)...")

  file(TO_CMAKE_PATH "$ENV{LD_LIBRARY_PATH}" LD_LIBRARY_PATH)

  find_library(PNETCDF_LIB
    NAMES pnetcdf
    HINTS ${LD_LIBRARY_PATH})

  if (PNETCDF_LIB)
	  get_filename_component(PNETCDF_LIB_DIR ${PNETCDF_LIB} PATH)
    string(REGEX REPLACE "/lib/?$" "/include"
	    PNETCDF_H_HINT ${PNETCDF_LIB_DIR})

    find_path (PNETCDF_INCLUDES pnetcdf.h
	    HINTS ${PNETCDF_H_HINT}
      DOC "Path to pnetcdf.h")
  endif()
endif()

if (PNETCDF_LIB AND PNETCDF_INCLUDES)
	set (PNETCDF_LIBRARIES "${PNETCDF_LIB}")
endif()

# handle the QUIETLY and REQUIRED arguments and set YAXT_FOUND to TRUE if
# all listed variables are TRUE
include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (PNETCDF DEFAULT_MSG PNETCDF_LIBRARIES PNETCDF_INCLUDES)

mark_as_advanced (PNETCDF_LIB PNETCDF_INCLUDES)
