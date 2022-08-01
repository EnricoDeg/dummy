# Find YAC
# Find YAC includes and library
#

if (YAC_INCLUDE_DIRS)
  # Already in cache, be silent
  set (YAC_FIND_QUIETLY TRUE)
endif (YAC_INCLUDE_DIRS)

include(FindPkgConfig)

pkg_check_modules(YAC REQUIRED yac)



#find_path (YAC_INCLUDES yac_interface.h
#	HINTS "${YAC_ROOT}/include" "$ENV{YAC_ROOT}/include")

#string(REGEX REPLACE "/include/?$" "/lib"
#	YAC_LIB_HINT ${YAC_INCLUDES})

#find_library (YAC_LIB
#  NAMES yac
#  HINTS ${YAC_LIB_HINT})

#if ((NOT YAC_LIB) OR (NOT YAC_INCLUDES))
#  message(STATUS "Trying to find YAC using LD_LIBRARY_PATH (we're desperate)...")

#  file(TO_CMAKE_PATH "$ENV{LD_LIBRARY_PATH}" LD_LIBRARY_PATH)

#  find_library(YAC_LIB
#    NAMES yac
#    HINTS ${LD_LIBRARY_PATH})

#  if (YAC_LIB)
#    get_filename_component(YAC_LIB_DIR ${YAC_LIB} PATH)
#    string(REGEX REPLACE "/lib/?$" "/include"
#	    YAC_H_HINT ${YAC_LIB_DIR})

#    find_path (YAC_INCLUDES yac_interface.h
#	    HINTS ${YAC_H_HINT}
#      DOC "Path to yac_interface.h")
#  endif()
#endif()
#
#if (YAC_LIB AND YAC_INCLUDES)
#	set (YAC_LIBRARIES "${YAC_LIB}")
#endif()

# handle the QUIETLY and REQUIRED arguments and set YAC_FOUND to TRUE if
# all listed variables are TRUE
#include (FindPackageHandleStandardArgs)
#find_package_handle_standard_args (YAC DEFAULT_MSG YAC_LIBRARIES YAC_INCLUDES)

#mark_as_advanced (YAC_LIB YAC_INCLUDES)
